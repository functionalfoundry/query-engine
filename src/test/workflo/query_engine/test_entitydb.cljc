(ns workflo.query-engine.test-entitydb
  (:require [environ.core :refer [env]]
            [workflo.entitydb.core :as entitydb]
            [workflo.entitydb.util.entities :as entities]
            [workflo.entitydb.util.operations :as ops]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.entitydb :as dl]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.test-entities]))


;;;; Temp ID resolution


(def +tempids+ (atom {}))


(defn initialize-tempid-map! []
  (reset! +tempids+ {}))


(defn resolve-tempid [conn tx id]
  (get (deref +tempids+) id))


(defn realize-tempid [tempid]
  (swap! +tempids+ update tempid (fn [real-id]
                                   (or real-id (entitydb/make-id))))
  (get (deref +tempids+) tempid))


;;;; Transact the initial test data


(defn realize-tempids-in-entity [data]
  (reduce (fn [data-out [k v]]
            (if (= :db/id k)
              (let [real-id (realize-tempid v)]
                (assoc data-out :db/id real-id :workflo/id real-id))
              (assoc data-out k
                     (cond
                       (and (map? v) (contains? v :db/id))
                       (let [real-id (realize-tempid (get v :db/id))]
                         {:workflo/id real-id})

                       (and (coll? v) (every? #(contains? % :db/id) v))
                       (into [] (map #(let [real-id (realize-tempid (get % :db/id))]
                                        {:workflo/id real-id}))
                             v)

                       :else v))))
          {} data))



(defn insert-data-into-map [db data]
  (let [db-config     (entitydb/db-config-from-registered-entities)
        realized-data (realize-tempids-in-entity data)
        entity-name   (entities/entity-name realized-data (get db-config :type-map))]
    (assert entity-name)
    (ops/add-entity db db-config entity-name realized-data)))


(defn transact-into-map [conn]
  (swap! conn (fn [db]
                (reduce #(insert-data-into-map %1 %2)
                        db test-data/map-data))))


;;;; (Atom) map setup


(def setup
  {:id-attr :workflo/id
   :ref-id-attr :workflo/id
   :connect (fn []
              (initialize-tempid-map!)
              (atom (entitydb/empty-db)))
   :db deref
   :db-config (entitydb/db-config-from-registered-entities)
   :transact transact-into-map
   :resolve-tempid resolve-tempid
   :data-layer dl/data-layer
   :new-cache #(atom-cache)})
