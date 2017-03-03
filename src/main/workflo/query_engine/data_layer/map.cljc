(ns workflo.query-engine.data-layer.map
  (:require [workflo.macros.entity :as entities]
            [workflo.macros.query.util :as query.util]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn resolve-refs [db id-attr entity-name refs]
  (into #{} (map (fn [ref]
                   (get-in db [entity-name (id-attr ref)])))
        refs))

(defn backref-attr->forward-attr [k]
  (keyword (namespace k) (subs (name k) 1)))

(defn resolve-backrefs [db id-attr backref forward-attr refs]
  (into #{} (filter (fn [source-data]
                      (when-let [ref-or-refs (get source-data forward-attr)]
                        (if (:many? backref)
                          (some (into #{} (map id-attr ref-or-refs))
                                (map id-attr refs))
                          (some #{(id-attr ref)}
                                (map id-attr refs))))))
        (vals (get db (:entity backref)))))

(defn resolve-path [db id-attr entity data-set path]
  (loop [data-entity (:name entity)
         data-set    data-set
         path        path]
    (if-not (seq path)
      data-set
      (let [attr            (first path)
            forward-attr    (backref-attr->forward-attr attr)
            entity-refs     (when-not (query.util/backref-attr? attr)
                              (entities/entity-refs data-entity))
            entity-backrefs (when (query.util/backref-attr? attr)
                              (entities/entity-backrefs data-entity))
            attr-ref        (get entity-refs attr)
            attr-backref    (get entity-backrefs forward-attr)]
        (cond
          attr-ref     (let [refs (if (:many? attr-ref)
                                    (into #{} (mapcat #(get % attr)) data-set)
                                    (into #{} (map #(get % attr)) data-set))]
                         (recur (:entity attr-ref)
                                (resolve-refs db id-attr (:entity attr-ref) refs)
                                (rest path)))
          attr-backref (recur (:entity attr-backref)
                              (resolve-backrefs db id-attr attr-backref
                                                forward-attr data-set)
                              (rest path))
          :else        (recur data-entity
                              (into #{} (map #(get % attr)) data-set)
                              (rest path)))))))

(defn matches-param? [db id-attr entity data param value]
  (let [path       (cond-> param
                     (not (sequential? param)) vector)
        result-set (resolve-path db id-attr entity [data] path)]
    (and (seq result-set)
         (some (fn [result-value]
                 (= result-value value))
               result-set))))

(defn matches-params? [db id-attr entity data params]
  (every? (fn [[param value]]
            (matches-param? db id-attr entity data param value))
          params))

(defn fetch-one-by-id
  [{:keys [db id-attr] :or {id-attr :db/id} :as env} entity id params]
  (when-let [data (get-in db [(:name entity) id])]
    (when (matches-params? db id-attr entity data (remove util/reserved-param? params))
      data)))

(defn fetch-entity
  [{:keys [cache] :as env} entity id params]
  (letfn [(fetch* [id]
            (fetch-one-by-id env entity id params))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn fetch-entities
  ([{:keys [db] :as env} entity params]
   (let [ids (keys (get db (:name entity)))]
     (fetch-entities env entity ids params)))
  ([{:keys [cache id-attr] :or {id-attr :db/id} :as env} entity ids params]
   (letfn [(fetch* [ids]
             (keep #(fetch-one-by-id env entity % params) ids))]
     (if cache
       (c/get-many cache ids
                   (fn [missing-ids]
                     (into {} (map (juxt id-attr identity))
                           (fetch* missing-ids))))
       (fetch* ids)))))

(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id params)
              (util/select-attrs attrs)
              (with-meta {:entity entity})))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids params)
               (transduce (comp (map #(util/select-attrs % attrs))
                                (map #(with-meta % {:entity entity})))
                          conj #{})
               (util/sort params)
               (util/paginate params)))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity params)
               (transduce (comp (map #(util/select-attrs % attrs))
                                (map #(with-meta % {:entity entity})))
                          conj #{})
               (util/sort params)
               (util/paginate params)))))

(comment

  (require 'workflo.query-engine.test-map)

  (def setup workflo.query-engine.test-map/setup)
  (def db (let [conn ((:connect setup)) 
                _    ((:transact setup) conn)]
            ((:db setup) conn)))

  (let [accounts (resolve-refs db :db/id 'account [{:db/id -1}])]
    (println "\n------ BEGIN ------")
    (resolve-path db :db/id 'account accounts [:account/users :account/_users :db/id]))

  (fetch-entities {:db db}
                  (entities/resolve-entity 'user)
                  [-10 -11 -12]
                  {[:user/account :db/id] -1})

  )
