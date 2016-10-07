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

(defn entity-from-join [parent attr]
  (let [parent-entity (:entity (meta parent))
        join-attr-ref (get (es/entity-refs parent-entity) attr)]
    (e/resolve-entity (:entity join-attr-ref))))

(defn singular-key? [k]
  (= (name k) (inflections/singular (name k))))

;;;; Data fetching

(defn fetch-entity-data [env entity singular? id-or-ids attrs params]
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

(defn fetch-keyword-data [env _ z ctx]
  (let [key (qz/query-key z)
        entity (entity-from-query-key key)
        data (fetch-entity-data env entity
                                (singular-key? key)
                                nil [] (:params ctx))]
    data))

(defn fetch-ident-data [env z attrs ctx]
  (let [key (zip/node (qz/ident-name z))
        value (zip/node (qz/ident-value z))
        entity (entity-from-query-key key)
        data (fetch-entity-data env entity true
                                (when (not= value '_)
                                  value)
                                attrs (:params ctx))]
    data))

(defn fetch-join-data [env parent-data z ctx]
  (let [join-source (qz/join-source z)
        join-query (qz/join-query z)
        attrs (loop [subz (qz/first-subquery join-query) attrs []]
                (let [new-attrs (conj attrs (qz/query-key subz))]
                  (if (qz/last-query? subz)
                    new-attrs
                    (recur (qz/next-query subz) new-attrs))))]
    (if-not parent-data
      (cond
        (keyword? (zip/node join-source))
        (let [key (qz/query-key join-source)
              entity (entity-from-query-key key)
              data (fetch-entity-data env entity (singular-key? key)
                                      (when (:db/id (:params ctx))
                                        (:db/id (:params ctx)))
                                      attrs (:params ctx))]
          data)

        (qz/ident-expr? join-source)
        (fetch-ident-data env join-source attrs ctx))
      (cond
        (keyword? (zip/node join-source))
        (let [key (qz/query-key join-source)
              entity (entity-from-join (zip/node parent-data) key)
              ref-or-refs (get (zip/node parent-data) key)
              singular? (and (map? ref-or-refs)
                             (:db/id ref-or-refs))
              data (fetch-entity-data env entity singular?
                                      (if singular?
                                        (:db/id ref-or-refs)
                                        (map :db/id ref-or-refs))
                                      attrs (:params ctx))]
          data)

        (qz/ident-expr? join-source)
        (fetch-ident-data env join-source attrs ctx)))))

;;;; Query processing

(defn process-query-node [env parent-data z ctx]
  (cond
    (keyword? (zip/node z))
    (fetch-keyword-data env parent-data z ctx)

    (qz/ident-expr? z)
    (fetch-ident-data env z [] ctx)

    (qz/join-expr? z)
    (fetch-join-data env parent-data z ctx)

    :else
    parent-data))

(defn process
  [query env]
  (zip/node (qz/process (qz/query-zipper query)
                        (partial process-query-node env)
                        (dz/data-zipper))))
