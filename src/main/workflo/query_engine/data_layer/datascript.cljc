(ns workflo.query-engine.data-layer.datascript
  (:require [workflo.macros.entity.schema :as es]
            [workflo.query-engine.cache :as c]
            [workflo.query-engine.data-layer :refer [DataLayer]]
            [datascript.core :as d]))

(defn- fetch-entity
  [{:keys [db cache]} entity id]
  (println "FETCH" id)
  (letfn [(fetch* [id]
            (println "GET" id "FROM DB")
            (d/pull db '[*] id))]
    (if cache
      (or (c/get-one cache id)
          (c/set-one cache id (fetch* id)))
      (fetch* id))))

(defn- fetch-entities
  ([{:keys [db cache] :as env} entity]
   (println "FETCH all of" (:name entity))
   (let [ids (d/q '[:find [?e ...]
                    :in $ [?a ...]
                    :where [?e ?a]]
                  db (es/required-keys entity))]
     (fetch-entities env entity ids)))
  ([{:keys [db cache]} entity ids]
   (println "FETCH" ids)
   (letfn [(fetch* [ids]
             (println "GET" ids "FROM DB")
             (d/q '[:find [(pull ?e [*]) ...]
                    :in $ [?e ...]]
                  db ids))]
     (if cache
       (c/get-many cache ids
                   (fn [missing-ids]
                     (into {} (map (fn [e] [(:db/id e) e]))
                           (fetch* missing-ids))))
       (fetch* ids)))))

(defn- filter-entity
  [data params]
  (when (and data
             params
             (every? (fn [[k v]]
                       (= (get data k) v))
                     params))
    data))

(defn- select-attrs
  [data attrs]
  (select-keys data attrs))

(defn data-layer []
  (reify DataLayer
    (fetch-one [_ env entity id params attrs]
      (some-> (fetch-entity env entity id)
              (filter-entity params)
              (select-attrs attrs)))
    (fetch-many [_ env entity ids params attrs]
      (some->> (fetch-entities env entity ids)
               (map #(filter-entity % params))
               (map #(select-attrs % attrs))
               (into #{})))
    (fetch-all [_ env entity params attrs]
      (some->> (fetch-entities env entity)
               (map #(filter-entity % params))
               (map #(select-attrs % attrs))
               (into #{})))))
