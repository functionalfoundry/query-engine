(ns workflo.query-engine.data-layer.datascript-no-authorization
  (:require [datascript.core :as d]
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
  ([{:keys [cache db] :as env} entity]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         rules     [(util/has-entity-attrs-rule req-attrs)]
         entities  (d/q '[:find [(pull ?e [*]) ...]
                          :in $ [?a ...] %
                          :where [?e ?a]
                                 (has-entity-attrs? ?e)]
                        db req-attrs rules)]
     (when (and cache (seq entities))
       (c/set-many cache (into {} (map (juxt :db/id identity)) entities)))
     entities))
  ([{:keys [db cache]} entity ids]
   (letfn [(fetch* [ids]
             (d/q '[:find [(pull ?e [*]) ...]
                    :in $ [?e ...]]
                  db ids entity))]
     (if cache
       (c/get-many cache ids
                   (fn [missing-ids]
                     (into {} (map (juxt :db/id identity))
                           (fetch* missing-ids))))
       (fetch* ids)))))

(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id)
              (util/filter-entity entity params)
              (util/select-attrs attrs)
              (with-meta {:entity entity})))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids)
               (transduce (comp (keep #(util/filter-entity % entity params))
                                (map #(util/select-attrs % attrs))
                                (map #(with-meta % {:entity entity})))
                          conj #{})
               (util/sort params)
               (util/paginate params)))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity)
               (transduce (comp (keep #(util/filter-entity % entity params))
                                (map #(util/select-attrs % attrs))
                                (map #(with-meta % {:entity entity})))
                          conj #{})
               (util/sort params)
               (util/paginate params)))))
