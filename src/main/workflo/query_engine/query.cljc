(ns workflo.query-engine.query
  (:require [clojure.zip :as zip]
            [inflections.core :as inflections]
            [om.next.impl.parser :as parser]
            [workflo.macros.entity :as e]
            [workflo.macros.entity.schema :as es]
            [workflo.query-engine.data-layer :as data-layer]
            [workflo.query-engine.query.data-zip :as dz]
            [workflo.query-engine.query.zip :as qz]))

;;;; Working with entities

(defn entity-from-query-key
  "Takes a query key (e.g. :users or :user) and resolves it
   it into the corresponding entity definition."
  [k]
  (or (try (e/resolve-entity (symbol (name k)))
           (catch #?(:cljs js/Error :clj Exception) e))
      (try (e/resolve-entity (symbol (inflections/singular (name k))))
           (catch #?(:cljs js/Error :clj Exception) e))))

(defn target-entity
  "Returns the target entity definition for a source entity and
   a ref attribute."
  [source attr]
  (let [source-entity (:entity (meta source))
        attr-ref (get (es/entity-refs source-entity) attr)]
    (e/resolve-entity (:entity attr-ref))))

(defn singular-key?
  "Returns whether or not a key is singular (e.g. :user,
   not :users)."
  [k]
  (= (name k) (inflections/singular (name k))))

;;;; Data fetching

(defn fetch-entity-data
  "Fetches one or more items of an entity from the data layer."
  [env entity singular? id-or-ids attrs params]
  (let [env (select-keys env [:cache :data-layer :db :viewer])
        attrs (into [:db/id] attrs)]
    (if singular?
      (if-let [id (or id-or-ids (:db/id params))]
        (data-layer/fetch-one (:data-layer env) env entity
                              id params attrs)
        (first (data-layer/fetch-all (:data-layer env) env entity
                                     params attrs)))
      (if id-or-ids
        (data-layer/fetch-many (:data-layer env) env entity
                               id-or-ids params attrs)
        (data-layer/fetch-all (:data-layer env) env entity
                              params attrs)))))

;;;; Query execution

(defn resolve-keyword
  [env _ z params]
  (let [key (qz/query-key z)
        entity (entity-from-query-key key)
        data (fetch-entity-data env entity
                                (singular-key? key)
                                nil [] params)]
    data))

(defn resolve-ident
  [env z attrs params]
  (let [key (zip/node (qz/ident-name z))
        value (zip/node (qz/ident-value z))
        entity (entity-from-query-key key)
        data (fetch-entity-data env entity true
                                (when (not= value '_)
                                  value)
                                attrs params)]
    data))

(defn attrs-from-join-query
  [z]
  (loop [subz (qz/first-subquery z) attrs []]
    (let [new-attrs (conj attrs (qz/query-key subz))]
      (if (qz/last-query? subz)
        new-attrs
        (recur (qz/next-query subz) new-attrs)))))

(defn resolve-toplevel-join
  [env join-source attrs params]
  (let [key (qz/query-key join-source)
        entity (entity-from-query-key key)]
    (fetch-entity-data env entity (singular-key? key)
                       (:db/id params)
                       attrs params)))

(defn resolve-nested-join
  [env parent-data join-source join-query attrs params]
  (let [key (qz/query-key join-source)
        entity (target-entity (zip/node parent-data) key)
        id-or-ids (let [ref-or-refs (get (zip/node parent-data) key)]
                    (if (map? ref-or-refs)
                      (:db/id ref-or-refs)
                      (map :db/id ref-or-refs)))
        singular? (not (coll? id-or-ids))]
    (fetch-entity-data env entity singular? id-or-ids attrs params)))

(defn resolve-join
  "Resolves a join query into the corresponding data given
   a parent data node (an entity map) and parameters."
  [env parent-data z params]
  (let [join-source (qz/join-source z)
        join-query (qz/join-query z)
        attrs (attrs-from-join-query join-query)]
    (cond
      (qz/ident-expr? join-source)
      (resolve-ident env join-source attrs params)

      (keyword? (zip/node join-source))
      (if-not parent-data
        (resolve-toplevel-join env join-source attrs params)
        (resolve-nested-join env parent-data
                             join-source join-query
                             attrs params)))))

(defn resolve-query-node
  "Resolves a query node z into the corresponding data given
   a parent data node and parameters."
  [env parent-data z params]
  (cond
    (keyword? (zip/node z)) (resolve-keyword env parent-data z params)
    (qz/ident-expr? z) (resolve-ident env z [] params)
    (qz/join-expr? z) (resolve-join env parent-data z params)
    :else parent-data))

(defn process
  "Processes an Om Next query given a a data layer."
  [query data-layer env]
  (let [env' (assoc env :data-layer data-layer)]
    (zip/node (qz/process (qz/query-zipper query)
                          (partial resolve-query-node env')
                          (dz/data-zipper)))))
