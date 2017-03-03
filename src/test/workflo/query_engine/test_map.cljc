(ns workflo.query-engine.test-map
  (:require [environ.core :refer [env]]
            [workflo.macros.entity :as entities]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.map :as dl]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.test-entities]))

;;;; Transact the initial test data

(defn entity-with-attr [attr]
  (first (keep (fn [[entity-name entity]]
                 (let [attrs (remove #{:db/id} (es/required-keys entity))]
                   (when (some #{attr} attrs)
                     entity)))
               (entities/registered-entities))))

(defn entity-for-data [data]
  (let [attrs (remove #{:db/id} (keys data))]
    (some entity-with-attr attrs)))

(defn insert-data-into-map [db data]
  (let [entity (entity-for-data data)]
    (assert entity)
    (assoc-in db [(:name entity) (:db/id data)] data)))

(defn transact-into-map [conn]
  (swap! conn (fn [db]
                (reduce insert-data-into-map db test-data/map-data))))

;;;; Temp ID resolution

(defn resolve-tempid [conn tx id]
  id)

;;;; (Atom) map setup

(def setup
  {:connect #(atom {})
   :db deref
   :transact transact-into-map
   :resolve-tempid resolve-tempid
   :data-layer dl/data-layer
   :new-cache #(atom-cache)})
