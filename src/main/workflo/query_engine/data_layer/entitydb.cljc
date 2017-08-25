(ns workflo.query-engine.data-layer.entitydb
  (:require [clojure.set :as set]
            [workflo.entitydb.core :as entitydb]
            [workflo.entitydb.util.query :as entitydb.query]
            [workflo.macros.entity :as entities]
            [workflo.macros.query.util :as query.util]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer fetch-many]]
            [workflo.query-engine.data-layer.util :as util]))


(defn resolve-entities-for-param
  [{:keys [db db-config] :as env} [param value]]
  (let [path   (cond-> param
                 (not (sequential? param)) vector)
        values (cond-> value
                 (not (set? value)) hash-set)]
    (entitydb.query/entities-for-attribute-path db db-config path values)))


(defn resolve-entities-for-params
  [env params]
  (reduce (fn [result-set param]
            (->> (resolve-entities-for-param env param)
                 (set/intersection result-set)))
          (resolve-entities-for-param env (first params))
          (rest params)))


(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (let [id-attr        (get env :id-attr :db/id)
            params-with-id (-> (into {} (remove util/reserved-param?) params)
                               (assoc id-attr id))]
        (some-> (resolve-entities-for-params env params-with-id)
                (first)
                (with-meta {:entity entity})
                (util/select-attrs attrs))))
    (fetch-many [_ env entity ids params attrs]
      (let [id-attr  (get env :id-attr :db/id)
            type-map (get-in env [:db-config :type-map])]
        (some->> (-> (into {} (remove util/reserved-param?) params)
                     (update id-attr (fn [existing-value]
                                       (or existing-value
                                           (set ids)))))
                 (resolve-entities-for-params env)
                 (into #{} (comp
                            ;; Only keep instances of this entity
                            (filter #(= (keyword (:name entity))
                                        (entitydb/entity-name % type-map)))
                            ;; Remove unrequested attributes
                            (map #(util/select-attrs % attrs))
                            ;; Add entity meta data
                            (map #(with-meta % {:entity entity}))))
                 (util/sort params)
                 (util/paginate params))))
    (fetch-all [this env entity params attrs]
      (let [db       (get env :db)
            ids      (into #{} (map first) (entitydb/entity-map db (keyword (:name entity))))]
        (fetch-many this env entity ids params attrs)))))
