(ns workflo.query-engine.test-datomic
  (:require [datomic.api :as d]
            [datomic-schema.schema :as ds]
            [environ.core :refer [env]]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.datomic :as ed]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.datomic :as dl]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.test-entities]))

;;;; Datomic schema

(def test-schema
  (->> (e/registered-entities)
       (vals)
       (map ed/entity-schema)
       (apply concat)))

;;;; Datomic fixture

(defn delete-db []
  (d/delete-database (env :datomic-uri)))

(defn create-db []
  (d/create-database (env :datomic-uri)))

(defn install-schema []
  (d/transact (d/connect (env :datomic-uri))
              (concat (ds/generate-parts [(ds/part "user")])
                      test-schema)))

(defn datomic-fixture [f]
  (delete-db)
  (create-db)
  (install-schema)
  (f)
  (delete-db))

;;;; Datomic setup

(def setup
  {:connect #(d/connect (env :datomic-uri))
   :db d/db
   :transact (fn [conn] @(d/transact conn test-data/datomic))
   :resolve-tempid (fn [conn tx id]
                     (d/resolve-tempid (d/db conn) (:tempids tx)
                                       (d/tempid :db.part/user id)))
   :data-layer dl/data-layer
   :new-cache #(atom-cache)})
