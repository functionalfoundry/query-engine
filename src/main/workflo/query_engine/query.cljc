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

(defn entity-from-query-key [k]
  (or (try (e/resolve-entity (symbol (name k)))
           (catch #?(:cljs js/Error :clj Exception) e))
      (try (e/resolve-entity (symbol (inflections/singular (name k))))
           (catch #?(:cljs js/Error :clj Exception) e))))

(defn singular-key? [k]
  (= (name k) (inflections/singular (name k))))

;;;; Data fetching

(defn fetch-entity-data [env entity singular? id-or-ids attrs params]
  (let [env (select-keys env [:cache :db :viewer])
        attrs (into [:db/id] attrs)]
    (if singular?
      (if-let [id (or id-or-ids (:db/id params))]
        (data-layer/fetch-one (:data-layer env)
                              {:db (:db env)
                               :cache (:cache env)
                               :viewer (:viewer env)}
                              entity id params attrs)))))

(defn fetch-keyword-data [env parent-data z ctx]
  (if (qz/toplevel? z)
    (let [key (qz/query-key z)
          entity (entity-from-query-key key)
          data (fetch-entity-data env entity
                                  (singular-key? key)
                                  nil [] (:params ctx))]
      (println "DATA" data)
      (zip/edit parent-data assoc key (:name entity)))
    parent-data))

(defn fetch-data [env parent-data z ctx]
  (cond
    (keyword? (zip/node z))
    (fetch-keyword-data env parent-data z ctx)

    :else
    parent-data))

;;;; Query processing

(defn process
  [query env]
  (zip/node (qz/process (qz/query-zipper query)
                        (partial fetch-data env)
                        (dz/data-zipper))))
