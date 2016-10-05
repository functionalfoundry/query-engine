(ns workflo.query-engine.test-datascript
  (:require [datascript.core :as d]
            [datomic-schema.schema :as ds]
            [environ.core :refer [env]]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.datascript :as ed]
            [workflo.query-engine.cache.atom :refer [atom-cache]]
            [workflo.query-engine.data-layer.datascript :as dl]
            [workflo.query-engine.test-data :as test-data]
            [workflo.query-engine.test-entities]))

;;;; DataScript schema

(def test-schema
  (->> (e/registered-entities)
       (vals)
       (map ed/entity-schema)
       (apply merge)))

;;;; DataScript setup

(def setup
  {:connect #(d/create-conn test-schema)
   :db d/db
   :transact (fn [conn] (d/transact! conn test-data/combined))
   :resolve-tempid (fn [conn tx id] ((:tempids tx) id))
   :data-layer dl/data-layer
   :new-cache #(atom-cache)})
