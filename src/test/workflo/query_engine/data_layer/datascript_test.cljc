(ns workflo.query-engine.data-layer.datascript-test
  (:require [clojure.test :refer [deftest]]
            [datascript.core :as d]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.datascript :as ed]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.datascript :as dl]
            [workflo.query-engine.test-entities]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.data-layer.common-test :as common]))

;;;; DataScript schema

(def test-schema
  (->> (e/registered-entities)
       (vals)
       (map ed/entity-schema)
       (apply merge)))

;;;; Common options

(def common-opts
  {:connect #(d/create-conn test-schema)
   :db d/db
   :transact (fn [conn] (d/transact! conn test-data/combined))
   :resolve-tempid (fn [conn tx id] ((:tempids tx) id))
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

