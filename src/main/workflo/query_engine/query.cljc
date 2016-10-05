(ns workflo.query-engine.query
  (:require [clojure.zip :as zip]
            [inflections.core :as inflections]
            [om.next.impl.parser :as parser]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.data-layer :as data-layer]))

;;;; Query zipper

(defn query-zipper [query]
  (zip/zipper (some-fn vector? map? seq?)
              seq
              (fn [node children]
                (let [ret (cond
                            (vector? node) (vec children)
                            (map? node) (into {} children)
                            (seq? node) children)]
                  (with-meta ret (meta node))))
              query))

(defn focus-join-source [query]
  (-> query zip/next zip/next))

(defn focus-join-query [query]
  (-> query zip/next zip/next zip/right))

;;;; Data zipper

(defn data-zipper []
  (zip/zipper (some-fn vector? map? set?)
              seq
              (fn [node children]
                (cond
                  (vector? node) (vec children)
                  (map? node) (into {} children)
                  (set? node) (into #{} children)))
              {}))

;;;; Utilities

(defn toplevel?
  [query]
  (nil? (zip/up (zip/up query))))

(defn join? [x]
  (and (map? x)
       (= 1 (count x))))

(defn link? [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))))

(defn parameterized? [x]
  (and (seq? x)
       (= 2 (count x))
       (map? (second x))))

(defn attr-query? [x]
  (or (keyword? x)
      (and (join? x)
           (not (link? (ffirst x))))))

(defn link-query? [x]
  (or (link? x)
      (and (join? x)
           (link? (ffirst x)))))

(defn unique-link? [x]
  (and (link? x)
       (= (second x) '_)))

(defn filter-query [pred query]
  (loop [query (zip/down query) ret []]
    (let [ret' (cond-> ret
                 (pred (zip/node query))
                 (conj query))]
      (if (zip/right query)
        (recur (zip/right query) ret')
        ret'))))

(defn query-key [x]
  (cond
    (keyword? x) x
    (join? x) (query-key (ffirst x))
    (link? x) (first x)))

(defn entity-from-keyword [kw]
  (or (try
        (e/resolve-entity (symbol (name kw)))
        (catch #?(:cljs js/Error :clj Exception) e))
      (try
        (e/resolve-entity (symbol (inflections/singular (name kw))))
        (catch #?(:cljs js/Error :clj Exception) e))))

;;;; Query processing

(defn ctx->env [ctx]
  {:cache (:cache ctx)
   :db (:db ctx)
   :viewer (:viewer ctx)})

(defn parent-attr-vals
  [parents attr]
  (transduce (map attr) conj #{} parents))

(defn focus-map-value
  [data key]
  (loop [data (zip/down data)]
    (if (= key (zip/node (zip/down data)))
      (zip/right (zip/down data))
      (when (zip/right data)
        (recur (zip/right data))))))

(defn unfocus-map-value
  [data]
  (zip/up (zip/up data)))

(defmulti process-expr
  (fn [query _ _]
    (let [depth (if (toplevel? query) :toplevel :nested)
          type (let [node (zip/node query)]
                 (cond
                   (keyword? node) :keyword
                   (join? node) :join
                   (link? node) :link
                   (parameterized? node) :parameterized
                   :else :query))]
      [depth type])))

(defmethod process-expr [:toplevel :join]
  [query data ctx]
  (println "PROCESS :toplevel :join" (zip/node query))
  (let [join-src (zip/node (focus-join-source query))
        join-query (focus-join-query query)
        attrs (map (comp query-key zip/node)
                   (filter-query attr-query? join-query))]
    (cond
      (keyword? join-src)
      (let [entity (entity-from-keyword join-src)]
        (assert entity
                (str "Querying for " join-src ", which cannot be "
                     "resolved to an entity definition."))
        (assert (not= (inflections/singular (name join-src))
                      (name join-src))
                (str "Querying for singular " join-src " without "
                     "a :db/id parameter."))
        (let [join-data (data-layer/fetch-all (:data-layer ctx)
                                              (ctx->env ctx)
                                              entity {} attrs)
              data' (-> data
                        (zip/edit assoc (query-key join-src) join-data)
                        (focus-map-value (query-key join-src)))]
          (unfocus-map-value
           (if (seq (zip/node data'))
             (loop [data* (zip/down data')]
               (let [new-data* (process-expr join-query data* ctx)]
                 (if (zip/right new-data*)
                   (recur (zip/right new-data*))
                   (zip/up new-data*))))
             data'))))

      (link? join-src)
      (let [join-entity (entity-from-keyword (first join-src))
            join-data (if (unique-link? join-src)
                        (first (data-layer/fetch-all (:data-layer ctx)
                                                     (ctx->env ctx)
                                                     join-entity
                                                     {} attrs))
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity
                                              (second join-src)
                                              {} attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (zip/node data')
           (process-expr join-query data' ctx)
           data'))))))

(defmethod process-expr [:nested :join]
  [query data ctx]
  (println "PROCESS :nested :join" (zip/node query))
  (let [join-src (zip/node (focus-join-source query))
        join-query (focus-join-query query)
        attrs (map (comp query-key zip/node)
                   (filter-query attr-query? join-query))]
    (cond
      (keyword? join-src)
      (let [parent (zip/node data)
            _ (println "DATA" data)
            parent-entity (:entity (meta parent))
            entity-ref (get (es/entity-refs parent-entity) join-src)
            join-entity (e/resolve-entity (:entity entity-ref))
            id-or-ids (if (:many? entity-ref)
                        (map :db/id (get parent join-src))
                        (:db/id (get parent join-src)))
            join-data (if (:many? entity-ref)
                        (data-layer/fetch-many (:data-layer ctx)
                                               (ctx->env ctx)
                                               join-entity id-or-ids
                                               {} attrs)
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity id-or-ids
                                              {} attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (zip/node data')
           (process-expr join-query data' ctx)
           data')))

      (link? join-src)
      (let [join-entity (entity-from-keyword (first join-src))
            join-data (if (unique-link? join-src)
                        (first (data-layer/fetch-all (:data-layer ctx)
                                                     (ctx->env ctx)
                                                     join-entity
                                                     {} attrs))
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity
                                              (second join-src)
                                              {} attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (zip/node data')
           (process-expr join-query data' ctx)
           data'))))))

(defmethod process-expr [:toplevel :link]
  [query data ctx]
  (println "PROCESS :toplevel :link" (zip/node query))
  data)

(defmethod process-expr [:nested :link]
  [query data ctx]
  (println "PROCESS :nested :link" (zip/node query))
  data)

(defmethod process-expr [:toplevel :parameterized]
  [query data ctx]
  (println "PROCESS :toplevel :parameterized" (zip/node query))
  data)

(defmethod process-expr [:nested :parameterized]
  [query data ctx]
  (println "PROCESS :nested :parameterized" (zip/node query))
  data)

(defmethod process-expr [:toplevel :keyword]
  [query data ctx]
  data)

(defmethod process-expr [:nested :keyword]
  [query data ctx]
  data)

(defn process-query
  [query data ctx]
  (loop [q (zip/down query) data data ctx ctx]
    (let [new-data (process-expr q data ctx)]
      (if (zip/right q)
        (recur (zip/right q) new-data ctx)
        new-data))))

(defmethod process-expr [:toplevel :query]
  [query data ctx]
  (process-query query data ctx))

(defmethod process-expr [:nested :query]
  [query data ctx]
  (process-query query data ctx))

(defn process [query ctx]
  (process-expr (query-zipper query) (data-zipper) ctx))

(comment
  (do
    (require '[workflo.query-engine.data-layer :as dlayer])
    (require '[environ.core :refer [env]])
    (require '[datomic.api :as d])
    (require '[datomic-schema.schema :as ds])
    (require '[workflo.macros.entity.datomic :as ed])
    (require '[workflo.macros.entity :as e])
    (require '[workflo.query-engine.test-entities])
    (require '[workflo.query-engine.test-data :as test-data])
    (require '[workflo.query-engine.cache.atom :refer [atom-cache]])
    (require '[workflo.query-engine.data-layer.datomic :as dl])
    (d/delete-database (env :datomic-uri))
    (d/create-database (env :datomic-uri))
    (def test-schema
      (->> (e/registered-entities)
           (vals)
           (map ed/entity-schema)
           (apply concat)))
    (d/transact (d/connect (env :datomic-uri))
                (concat (ds/generate-parts [(ds/part "user")])
                        test-schema))
    (def conn (d/connect (env :datomic-uri)))
    (def tx @(d/transact conn test-data/datomic))
    (defn realid [id]
      (d/resolve-tempid (d/db conn) (:tempids tx)
                        (d/tempid :db.part/user id)))
    (def datomic-layer (dl/data-layer))
    (def joe (realid -10))
    (def linda (realid -12)))

  (do
    (def query `[{:components [:db/id :component/name
                               {:component/states
                                [:component-state/name]}
                               {[:component-state ~(realid -10001)]
                                [:db/id :component-state/name
                                 {:component-state/component
                                  [:component/name]}]}]}
                 {:component-states [:db/id :component-state/name]}
                 {[:component-state ~(realid -10002)]
                  [:db/id :component-state/name
                   {:component-state/component
                    [:component/name]}]}])
    ;;(:users {:db/id ~(realid -10)})
    (clojure.pprint/pprint
     (process query {:data-layer datomic-layer
                     :cache (atom-cache)
                     :db (d/db conn)
                     :viewer linda})))

  )
