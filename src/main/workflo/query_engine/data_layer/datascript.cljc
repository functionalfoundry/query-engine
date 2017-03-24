(ns workflo.query-engine.data-layer.datascript
  (:require [datascript.core :as d]
            [workflo.macros.entity :as entities]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn authorized?
  [db entity e viewer]
  (entities/authorized? entity {:db db} e viewer))

(defn- fetch-entity
  [{:keys [cache db id-attr skip-authorization? viewer]
    :or {id-attr :db/id}
    :as env} entity id params]
  (letfn [(fetch* [id]
            (if (= :db/id id-attr)
              (if skip-authorization?
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?e %
                       :where (matches-params? ?e)]
                     db id
                     (util/matches-params-rules* params))
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?e ?entity ?viewer ?authorized %
                       :where (matches-params? ?e)
                              [(?authorized $ ?entity ?e ?viewer)]]
                     db id entity viewer authorized?
                     (util/matches-params-rules* params)))
              (if skip-authorization?
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?id-attr ?id ?entity %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)]
                     db id-attr id
                     (util/matches-params-rules* params))
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?id-attr ?id ?entity ?viewer ?authorized %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(?authorized $ ?entity ?id ?viewer)]]
                     db id-attr id entity viewer authorized?
                     (util/matches-params-rules* params)))))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [cache db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity params]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         rules     (conj (util/matches-params-rules* params)
                         (util/has-entity-attrs-rule req-attrs))
         entities  (if (= :db/id id-attr)
                     (if skip-authorization?
                       (d/q '[:find [(pull ?e [*]) ...]
                              :in $ %
                              :where (matches-params? ?e)
                                     (has-entity-attrs? ?e)]
                            db rules)
                       (d/q '[:find [(pull ?e [*]) ...]
                              :in $ ?entity ?viewer ?authorized %
                              :where (matches-params? ?e)
                                     (has-entity-attrs? ?e)
                                     [(?authorized $ ?entity ?e ?viewer)]]
                            db entity viewer authorized? rules))
                     (if skip-authorization?
                       (d/q '[:find [(pull ?e [*]) ...]
                              :in $ %
                              :where (matches-params? ?e)
                                     (has-entity-attrs? ?e)]
                            db rules)
                       (d/q '[:find [(pull ?e [*]) ...]
                              :in $ ?id-attr ?entity ?viewer ?authorized %
                              :where (matches-params? ?e)
                                     (has-entity-attrs? ?e)
                                     [?e ?id-attr ?id]
                                     [(?authorized $ ?entity ?id ?viewer)]]
                            db id-attr entity viewer authorized? rules)))]
     (when (and cache (seq entities))
       (c/set-many cache (into {} (map (juxt id-attr identity)) entities)))
     entities))
  ([{:keys [cache db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity ids params]
   (letfn [(fetch* [ids]
             (if (= :db/id id-attr)
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...] %
                        :where (matches-params? ?e)]
                        db ids
                        (util/matches-params-rules* params))
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...] ?entity ?viewer ?authorized %
                        :where (matches-params? ?e)
                               [(?authorized $ ?entity ?e ?viewer)]]
                      db ids entity viewer authorized?
                      (util/matches-params-rules* params)))
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...] %
                        :where [?e ?id-attr ?id]
                               (matches-params ?e)]
                      db id-attr ids
                      (util/matches-params-rules* params))
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...] ?entity ?viewer ?authorized %
                        :where [?e ?id-attr ?id]
                               (matches-params? ?e)
                               [(?authorized $ ?entity ?id ?viewer)]]
                      db id-attr ids entity viewer authorized?
                      (util/matches-params-rules* params)))))]
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
