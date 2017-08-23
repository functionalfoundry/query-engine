(ns workflo.query-engine.data-layer.entitydb
  (:require [clojure.set :as set]
            [workflo.entitydb.core :as entitydb]
            [workflo.entitydb.util.query :as entitydb.query]
            [workflo.macros.entity :as entities]
            [workflo.macros.query.util :as query.util]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
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


(defn fetch-many-by-params
  [{:keys [db db-config] :as env} entity ids params]
  (let [type-map (get db-config :type-map)
        entities (resolve-entities-for-params env params)]
    (cond->> entities
      ;; Filter entities by their entity name
      (not (nil? entity))
      (filter #(= (keyword (:name entity))
                  (entitydb/entity-name % type-map)))

      ;; Only return entities that match the input IDs (if
      ;; there are any being passed in)
      (not (empty? ids))
      (filter #(some #{(get % :workflo/id)} ids))

      ;; Add entity meta data and return the result as a set
      true
      (into #{} (map #(with-meta % {:entity entity}))))))


(defn fetch-entity
  [env entity id params]
  (let [params-with-id (conj params [:workflo/id id])]
    (first (fetch-many-by-params env entity #{id} params-with-id))))


(defn fetch-entities
  ([{:keys [db] :as env} entity params]
   (if (empty? params)
     (let [ids (keys (entitydb/entity-map db (keyword (:name entity))))]
       (keep #(fetch-entity env entity % nil) ids))
     (fetch-many-by-params env entity nil params)))
  ([env entity ids params]
   (if (empty? params)
     (keep #(fetch-entity env entity % nil) ids)
     (fetch-many-by-params env entity ids params))))


(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id (remove util/reserved-param? params))
              (util/select-attrs attrs)))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids (remove util/reserved-param? params))
               (into #{} (map #(util/select-attrs % attrs)))
               (util/sort params)
               (util/paginate params)))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity (remove util/reserved-param? params))
               (into #{} (map #(util/select-attrs % attrs)))
               (util/sort params)
               (util/paginate params)))))
