(ns workflo.query-engine.data-layer.datomic-test
  (:require [clojure.test :refer [deftest use-fixtures]]
            [datomic.api :as d]
            [datomic-schema.schema :as ds]
            [environ.core :refer [env]]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.datomic :as ed]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.datomic :as dl]
            [workflo.query-engine.test-entities]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.data-layer.common-test :as common]))

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

(use-fixtures :each datomic-fixture)

;;;; Common options

(def common-opts
  {:connect #(d/connect (env :datomic-uri))
   :db d/db
   :transact (fn [conn] @(d/transact conn test-data/datomic))
   :resolve-tempid (fn [conn tx id]
                     (d/resolve-tempid (d/db conn) (:tempids tx)
                                       (d/tempid :db.part/user id)))
   :data-layer dl/data-layer})

;;;; Fetch individual entities

(deftest fetch-one-without-cache
  (common/test-fetch-one (assoc common-opts :cache nil)))

(deftest fetch-one-with-cache
  (common/test-fetch-one (assoc common-opts :cache (atom-cache))))

;;;; Fetch a collection of entities

(deftest fetch-many-without-cache
  (common/test-fetch-many (assoc common-opts :cache nil)))

(deftest fetch-many-with-cache
  (common/test-fetch-many (assoc common-opts :cache (atom-cache))))

;;;; Fetch all instances of an entity

(deftest fetch-all-without-cache
  (common/test-fetch-all (assoc common-opts :cache nil)))

(deftest fetch-all-with-cache
  (common/test-fetch-all (assoc common-opts :cache (atom-cache))))
