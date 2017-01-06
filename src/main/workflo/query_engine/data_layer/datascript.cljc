(ns workflo.query-engine.data-layer.datascript
  (:require [datascript.core :as d]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [workflo.query-engine.data-layer.util :as util]))

(defn authorized?
  [db entity e viewer]
  (if-let [auth-rules (some-> (:auth entity) (apply [{}]))]
    (let [results (mapv (fn [auth-rule]
                          (d/q '[:find ?e
                                 :in $ ?e ?viewer %
                                 :where (auth ?e ?viewer)]
                               db e viewer [auth-rule]))
                        auth-rules)]
      (not (empty? (apply concat results))))
    true))

(defn- fetch-entity
  [{:keys [cache db id-attr skip-authorization? viewer]
    :or {id-attr :db/id}
    :as env} entity id]
  (letfn [(fetch* [id]
            (if (= :db/id id-attr)
              (if skip-authorization?
                (d/pull db '[*] id)
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?e ?entity ?viewer ?authorized
                       :where [(?authorized $ ?entity ?e ?viewer)]]
                     db id entity viewer authorized?))
              (if skip-authorization?
                (d/pull db '[*] [id-attr id])
                (d/q '[:find (pull ?e [*]) .
                       :in $ ?id-attr ?id ?entity ?viewer ?authorized
                       :where [?e ?id-attr ?id]
                              [(?authorized $ ?entity ?e ?viewer)]]
                     db id-attr id entity viewer authorized?))))]
    (if cache
      (c/get-one cache id fetch*)
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [cache db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity]
   (let [req-attrs (remove #{:db/id} (es/required-keys entity))
         rules     [(util/has-entity-attrs-rule req-attrs)]
         entities  (if skip-authorization?
                     (d/q '[:find [(pull ?e [*]) ...]
                            :in $ [?a ...] %
                            :where [?e ?a]
                                   (has-entity-attrs? ?e)]
                          db req-attrs rules)
                     (d/q '[:find [(pull ?e [*]) ...]
                            :in $ [?a ...] ?entity ?viewer ?authorized %
                            :where [?e ?a]
                                   (has-entity-attrs? ?e)
                                   [(?authorized $ ?entity ?e ?viewer)]]
                          db req-attrs entity viewer authorized? rules))]
     (when (and cache (seq entities))
       (c/set-many cache (into {} (map (juxt id-attr identity)) entities)))
     entities))
  ([{:keys [cache db id-attr skip-authorization? viewer]
     :or {id-attr :db/id}
     :as env} entity ids]
   (letfn [(fetch* [ids]
             (if (= :db/id id-attr)
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...]]
                      db ids entity)
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ [?e ...] ?entity ?viewer ?authorized
                        :where [(?authorized $ ?entity ?e ?viewer)]]
                      db ids entity viewer authorized?))
               (if skip-authorization?
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...]
                        :where [?e ?id-attr ?id]]
                      db id-attr ids entity)
                 (d/q '[:find [(pull ?e [*]) ...]
                        :in $ ?id-attr [?id ...] ?entity ?viewer ?authorized
                        :where [?e ?id-attr ?id]
                               [(?authorized $ ?entity ?e ?viewer)]]
                      db id-attr ids entity viewer authorized?))))]
     (if cache
       (c/get-many cache ids
                   (fn [missing-ids]
                     (into {} (map (juxt id-attr identity))
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
