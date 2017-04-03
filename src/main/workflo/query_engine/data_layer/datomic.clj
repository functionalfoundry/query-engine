(ns workflo.query-engine.data-layer.datomic
  (:require [datomic.api :as d]
            [workflo.macros.entity :as entities]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn authorized?
  [db entity entity-id viewer-id skip-authorization?]
  (or skip-authorization?
      (entities/authorized? entity {:db db} entity-id viewer-id)))

(defn- fetch-entity
  [{:keys [db cache following-ref? id-attr ref-id-attr skip-authorization? viewer]
    :or {id-attr :db/id
         ref-id-attr :db/id
         skip-authorization? false}
    :as env} entity id params]
  (letfn [(fetch* [id]
            (if following-ref?
              (cond
                (and (= :db/id id-attr)
                     (= :db/id ref-id-attr))
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?e
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (= :db/id id-attr)
                     (not= :db/id ref-id-attr))
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?ref-id-attr
                           ?ref-id
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?ref-id-attr ?ref-id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db ref-id-attr id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (not= :db/id id-attr)
                     (= :db/id ref-id-attr))
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?id-attr
                           ?e
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (not= :db/id id-attr)
                     (not= :db/id ref-id-attr))
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?id-attr
                           ?ref-id-attr
                           ?ref-id
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?ref-id-attr ?ref-id]
                              [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr ref-id-attr id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params)))
              (cond
                (= :db/id id-attr)
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?e
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (not= :db/id id-attr)
                (d/q '[:find (pull ?e [*]) .
                       :in $
                           ?id-attr
                           ?id
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr id entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params)))))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [db following-ref? id-attr ref-id-attr skip-authorization? viewer]
     :or {id-attr :db/id
          ref-id-attr :db/id
          skip-authorization? false}
     :as env} entity params]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         rules     (conj (util/matches-params-rules* params)
                         (util/has-entity-attrs-rule req-attrs))
         ids       (cond
                     (= :db/id id-attr)
                     (d/q '[:find [?e ...]
                            :in $
                                [?a ...]
                                ?entity
                                ?viewer
                                ?skip-authorization
                                %
                            :where [?e ?a]
                                   (matches-params? ?e)
                                   (has-entity-attrs? ?e)
                                   [(workflo.query-engine.data-layer.datomic/authorized?
                                     $ ?entity ?e ?viewer ?skip-authorization)]]
                          db req-attrs entity
                          (if skip-authorization? :undefined viewer)
                          skip-authorization? rules)

                     (not= :db/id id-attr)
                     (d/q '[:find [?id ...]
                            :in $
                                ?id-attr
                                [?a ...]
                                ?entity
                                ?viewer
                                ?skip-authorization
                                %
                            :where [?e ?a]
                                   (matches-params? ?e)
                                   (has-entity-attrs? ?e)
                                   [?e ?id-attr ?id]
                                   [(workflo.query-engine.data-layer.datomic/authorized?
                                     $ ?entity ?id ?viewer ?skip-authorization)]]
                          db id-attr req-attrs entity
                          (if skip-authorization? :undefined viewer)
                          skip-authorization? rules))]
     (fetch-entities (assoc env :ref-id-attr id-attr) entity ids params)))
  ([{:keys [cache db following-ref? id-attr ref-id-attr skip-authorization? viewer]
     :or {id-attr :db/id
          ref-id-attr :db/id
          skip-authorization? false}
     :as env} entity ids params]
   (letfn [(fetch* [ids]
             (if following-ref?
               (cond
                (and (= :db/id id-attr)
                     (= :db/id ref-id-attr))
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           [?e ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (= :db/id id-attr)
                     (not= :db/id ref-id-attr))
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           ?ref-id-attr
                           [?ref-id ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?ref-id-attr ?ref-id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db ref-id-attr ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (not= :db/id id-attr)
                     (= :db/id ref-id-attr))
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           ?id-attr
                           [?e ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (and (not= :db/id id-attr)
                     (not= :db/id ref-id-attr))
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           ?id-attr
                           ?ref-id-attr
                           [?ref-id ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?ref-id-attr ?ref-id]
                              [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr ref-id-attr ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params)))
              (cond
                (= :db/id id-attr)
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           [?e ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?e ?viewer ?skip-authorization)]]
                     db ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
                     (util/matches-params-rules* params))

                (not= :db/id id-attr)
                (d/q '[:find [(pull ?e [*]) ...]
                       :in $
                           ?id-attr
                           [?id ...]
                           ?entity
                           ?viewer
                           ?skip-authorization
                           %
                       :where [?e ?id-attr ?id]
                              (matches-params? ?e)
                              [(workflo.query-engine.data-layer.datomic/authorized?
                                $ ?entity ?id ?viewer ?skip-authorization)]]
                     db id-attr ids entity
                     (if skip-authorization? :undefined viewer)
                     skip-authorization?
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
