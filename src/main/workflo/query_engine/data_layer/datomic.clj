(ns workflo.query-engine.data-layer.datomic
  (:require [datomic.api :as d]
            [workflo.macros.entity :as entities]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn authorized?
  [db entity entity-id viewer-id]
  (entities/authorized? entity {:db db} entity-id viewer-id))

(defn- fetch-entity
  [{:keys [db cache id-attr skip-authorization? viewer]
    :or {id-attr :db/id}
    :as env} entity id params]
  (letfn [(fetch* [id]
            (if (= :db/id id-attr)
              (if skip-authorization?
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?e %
                       :where (matches-params? ?e)]
                     db id (util/matches-params-rules* params))
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?e ?entity ?viewer %
                       :where (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer)]]
                     db id entity viewer (util/matches-params-rules* params)))
              (if skip-authorization?
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?id-attr ?id %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)]
                     db id-attr id (util/matches-params-rules* params))
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?id-attr ?id ?entity ?viewer %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer)]]
                     db id-attr id entity viewer
                     (util/matches-params-rules* params)))))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity params]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         rules     (conj (util/matches-params-rules* params)
                         (util/has-entity-attrs-rule req-attrs))
         ids       (if (= :db/id id-attr)
                     (if skip-authorization?
                       (d/q '[:find [?e ...]
                              :in $ [?a ...] %
                              :where [?e ?a]
                                     (matches-params? ?e)
                                     (has-entity-attrs? ?e)]
                            db req-attrs rules)
                       (d/q '[:find [?e ...]
                              :in $ [?a ...] ?entity ?viewer %
                              :where [?e ?a]
                                     (matches-params? ?e)
                                     (has-entity-attrs? ?e)
                                     [(workflo.query-engine.data-layer.datomic/authorized?
                                       $ ?entity ?e ?viewer)]]
                            db req-attrs entity viewer rules))
                     (if skip-authorization?
                       (d/q '[:find [?id ...]
                              :in $ ?id-attr [?a ...] %
                              :where [?e ?id-attr ?id]
                                     [?e ?a]
                                     (matches-params? ?e)
                                     (has-entity-attrs? ?e)]
                            db id-attr req-attrs rules)
                       (d/q '[:find [?id ...]
                              :in $ ?id-attr [?a ...] ?entity ?viewer %
                              :where [?e ?id-attr ?id]
                                     [?e ?a]
                                     (matches-params? ?e)
                                     (has-entity-attrs? ?e)
                                     [(workflo.query-engine.data-layer.datomic/authorized?
                                       $ ?entity ?id ?viewer)]]
                            db id-attr req-attrs entity viewer rules)))]
     (fetch-entities env entity ids params)))
  ([{:keys [cache db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity ids params]
   (letfn [(fetch* [ids]
             (if (= :db/id id-attr)
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...] %
                        :where (matches-params? ?e)]
                      db ids (util/matches-params-rules* params))
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...] ?entity ?viewer %
                        :where (matches-params? ?e)
                               [(workflo.query-engine.data-layer.datomic/authorized?
                                 $ ?entity ?e ?viewer)]]
                      db ids entity viewer (util/matches-params-rules* params)))
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...] %
                        :where [?e ?id-attr ?id]]
                      db id-attr ids (util/matches-params-rules* params))
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...] ?entity ?viewer %
                        :where [?e ?id-attr ?id]
                               (matches-params? ?e)
                               [(workflo.query-engine.data-layer.datomic/authorized?
                                 $ ?entity ?id ?viewer)]]
                      db id-attr ids entity viewer
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
