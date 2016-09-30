(ns workflo.query-engine.data-layer.datomic
  (:require [datomic.api :as d]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn- fetch-entity
  [{:keys [db cache]} entity id]
  (letfn [(fetch* [id]
            (d/pull db '[*] id))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [db] :as env} entity]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         ids (d/q '[:find [?e ...]
                    :in $ [?a ...]
                    :where [?e ?a]]
                  db req-attrs)]
     (fetch-entities env entity ids)))
  ([{:keys [db cache]} entity ids]
   (letfn [(fetch* [ids]
             (d/q '[:find [(pull ?e [*]) ...]
                    :in $ [?e ...]]
                  db ids))]
     (if cache
       (c/get-many cache ids
                   (fn [missing-ids]
                     (into {} (map (fn [e] [(:db/id e) e]))
                           (fetch* missing-ids))))
       (fetch* ids)))))

(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id)
              (util/filter-entity params)
              (util/select-attrs attrs)))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids)
               (map #(util/filter-entity % params))
               (map #(util/select-attrs % attrs))
               (util/sort params)
               (util/paginate params)))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity)
               (map #(util/filter-entity % params))
               (map #(util/select-attrs % attrs))
               (util/sort params)
               (util/paginate params)))))
