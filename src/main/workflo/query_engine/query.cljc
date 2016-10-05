(ns workflo.query-engine.query
  (:require [clojure.zip :as zip]
            [inflections.core :as inflections]
            [om.next.impl.parser :as parser]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.data-layer :as data-layer]))

;;;; Forward declarations

(declare process-query)

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

(defn focus-parameterized-query [query]
  (-> query zip/down))

(defn focus-parameterized-params [query]
  (-> query zip/down zip/right))

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

(defn singular? [kw]
  (= (inflections/singular (name kw))
     (name kw)))

(defn join? [x]
  (and (map? x)
       (= 1 (count x))))

(defn link? [x]
  (and (vector? x)
       (= 2 (count x))
       (keyword? (first x))
       #_(or (= (second x) '_)
           (number? (second x)))))

(defn parameterized? [x]
  (and (seq? x)
       (= 2 (count x))
       (map? (second x))))

(defn toplevel?
  [query]
  (or (nil? (zip/up (zip/up query)))
      (and (parameterized? (zip/node (zip/up query)))
           (toplevel? (zip/up query)))))

(defn attr-query? [x]
  (or (keyword? x)
      (and (join? x)
           (not (link? (ffirst x))))
      (and (parameterized? x)
           (attr-query? (first x)))))

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
    (link? x) (first x)
    (parameterized? x) (query-key (first x))))

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

(defn query-level-and-type
  [query _ _ _]
  (let [level (if (toplevel? query) :toplevel :nested)
        type (let [node (zip/node query)]
               (cond
                 (keyword? node) :keyword
                 (join? node) :join
                 (link? node) :link
                 (parameterized? node) :parameterized
                 :else :query))]
    [level type]))

(defmulti process-expr query-level-and-type)

(defmethod process-expr [:toplevel :join]
  [query params data ctx]
  (println ">> :toplevel :join" (zip/node query))
  (let [join-src (zip/node (focus-join-source query))
        join-query (focus-join-query query)
        attrs (mapv (comp query-key zip/node)
                    (filter-query attr-query? join-query))]
    (cond
      (keyword? join-src)
      (let [entity (entity-from-keyword join-src)]
        (if (singular? join-src)
          (let [join-data (if (:db/id params)
                            (data-layer/fetch-one (:data-layer ctx)
                                                  (ctx->env ctx)
                                                  entity (:db/id params)
                                                  params attrs)
                            (first
                             (data-layer/fetch-all (:data-layer ctx)
                                                   (ctx->env ctx)
                                                   entity params attrs)))
                data' (-> data
                          (zip/edit assoc (query-key join-src) join-data)
                          (focus-map-value (query-key join-src)))]
            (println "DATA'" (zip/node data'))
            (unfocus-map-value
             (if (seq (zip/node data'))
               (loop [data* (zip/down data')]
                 (let [new-data* (process-query join-query {}
                                                data* ctx)]
                   (if (zip/right new-data*)
                     (recur (zip/right new-data*))
                     (zip/up new-data*))))
               (if (zip/node data')
                 (process-query join-query {} data' ctx)
                 data'))))
          (let [join-data (data-layer/fetch-all (:data-layer ctx)
                                                (ctx->env ctx)
                                                entity params attrs)
                data' (-> data
                          (zip/edit assoc (query-key join-src) join-data)
                          (focus-map-value (query-key join-src)))]
            (unfocus-map-value
             (if (seq (zip/node data'))
               (loop [data* (zip/down data')]
                 (let [new-data* (process-query join-query {}
                                                data* ctx)]
                   (if (zip/right new-data*)
                     (recur (zip/right new-data*))
                     (zip/up new-data*))))
               (if (zip/node data')
                 (process-query join-query {} data' ctx)
                 data'))))))

      (link? join-src)
      (let [join-entity (entity-from-keyword (first join-src))
            join-data (if (unique-link? join-src)
                        (first (data-layer/fetch-all (:data-layer ctx)
                                                     (ctx->env ctx)
                                                     join-entity
                                                     params attrs))
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity
                                              (second join-src)
                                              params attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (zip/node data')
           (process-query join-query {} data' ctx)
           data'))))))

(defmethod process-expr [:nested :join]
  [query params data ctx]
  (println ">> :nested :join" (zip/node query))
  (let [join-src (zip/node (focus-join-source query))
        join-query (focus-join-query query)
        attrs (mapv (comp query-key zip/node)
                    (filter-query attr-query? join-query))]
    (cond
      (keyword? join-src)
      (let [parent (zip/node data)
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
                                               params attrs)
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity id-or-ids
                                              params attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (seq (zip/node data'))
           (loop [data* (zip/down data')]
             (let [new-data* (process-query join-query {} data* ctx)]
               (if (zip/right new-data*)
                 (recur (zip/right new-data*))
                 (zip/up new-data*))))
           (if (zip/node data')
             (process-expr join-query {} data' ctx)
             data'))))

      (link? join-src)
      (let [join-entity (entity-from-keyword (first join-src))
            join-data (if (unique-link? join-src)
                        (first (data-layer/fetch-all (:data-layer ctx)
                                                     (ctx->env ctx)
                                                     join-entity
                                                     params attrs))
                        (data-layer/fetch-one (:data-layer ctx)
                                              (ctx->env ctx)
                                              join-entity
                                              (second join-src)
                                              params attrs))
            data' (-> data
                      (zip/edit assoc (query-key join-src) join-data)
                      (focus-map-value (query-key join-src)))]
        (unfocus-map-value
         (if (zip/node data')
           (process-query join-query {} data' ctx)
           data'))))))

(defmethod process-expr [:toplevel :link]
  [query params data ctx]
  (println ">> :toplevel :link" (zip/node query))
  (let [link (zip/node query)
        entity (entity-from-keyword (first link))
        link-data (if (unique-link? link)
                    (first (data-layer/fetch-all (:data-layer ctx)
                                                 (ctx->env ctx)
                                                 entity params [:db/id]))
                    (data-layer/fetch-one (:data-layer ctx)
                                          (ctx->env ctx)
                                          entity (second link)
                                          params [:db/id]))]
    (zip/edit data assoc (query-key link) link-data)))

(defmethod process-expr [:nested :link]
  [query params data ctx]
  (println ">> :nested :link" (zip/node query))
  data)

(defmethod process-expr [:toplevel :parameterized]
  [query _ data ctx]
  (println ">> :toplevel :parameterized" (zip/node query))
  (let [subquery (focus-parameterized-query query)
        params (zip/node (focus-parameterized-params query))]
    (process-expr subquery params data ctx)))

(defmethod process-expr [:nested :parameterized]
  [query params data ctx]
  (println ">> :nested :parameterized" (zip/node query))
  (let [subquery (focus-parameterized-query query)
        params (zip/node (focus-parameterized-params query))]
    (process-expr subquery params data ctx)))

(defmethod process-expr [:toplevel :keyword]
  [query params data ctx]
  (println ">> :toplevel :keyword" (zip/node query))
  (let [keyword (zip/node query)
        entity (entity-from-keyword keyword)
        kw-data (if (singular? keyword)
                  (if (:db/id params)
                    (data-layer/fetch-one (:data-layer ctx)
                                          (ctx->env ctx)
                                          entity (:db/id params)
                                          params [:db/id])
                    (first (data-layer/fetch-all (:data-layer ctx)
                                                 (ctx->env ctx)
                                                 entity params [:db/id])))
                  (data-layer/fetch-all (:data-layer ctx)
                                        (ctx->env ctx)
                                        entity params [:db/id]))]
    (zip/edit data assoc keyword kw-data)))

(defmethod process-expr [:nested :keyword]
  [query _ data ctx]
  data)

(defn process-query
  [query _ data ctx]
  (loop [q (zip/down query) data data ctx ctx]
    (let [new-data (process-expr q {} data ctx)]
      (if (zip/right q)
        (recur (zip/right q) new-data ctx)
        new-data))))

(defmethod process-expr [:toplevel :query]
  [query _ data ctx]
  (process-query query {} data ctx))

(defmethod process-expr [:nested :query]
  [query _ data ctx]
  (process-query query {} data ctx))

(defn process [query ctx]
  (zip/node (process-expr (query-zipper query) {} (data-zipper) ctx)))

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
