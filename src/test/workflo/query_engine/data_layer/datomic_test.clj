(ns workflo.query-engine.data-layer.datomic-test
  (:require [clojure.spec :as s]
            [clojure.test :refer [deftest is use-fixtures]]
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

(ds/defdbfn update-entity [db data spec] :db.part/user
  (let [entity (merge (datomic.api/pull db '[*] (:db/id data)) data)]
      (println "ENTITY" data entity)
      (if (clojure.spec/valid? spec entity)
        [data]
        (throw (RuntimeException.
                (clojure.spec/explain-str spec data))))))

;;;; Datomic fixture

(defn delete-db []
  (d/delete-database (env :datomic-uri)))

(defn create-db []
  (d/create-database (env :datomic-uri)))

(defn install-schema []
  (d/transact (d/connect (env :datomic-uri))
              (concat (ds/generate-parts [(ds/part "user")])
                      test-schema
                      (ds/dbfns->datomic update-entity))))

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

;;;; Transact with validation

(defn test-transact-with-validation
  [{:keys [connect db data-layer transact resolve-tempid]}]
  (println "Test transacting with validation")
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)]
    @(d/transact conn
                 [[:update-entity
                   {:db/id (d/tempid :db.part/user -999)
                    :account/name "Incomplete account"
                    :account/users []}
                   (test-entity-specs)]])))

(deftest transact-with-validation
  (test-transact-with-validation common-opts))
