(ns workflo.query-engine.query.zip
  (:require [clojure.zip :as zip]))

;;;; Forward declarations

(declare query-expr?)

;;;; Query zipper

(defn query-zipper [query]
  (zip/zipper (some-fn vector? map? seq?)
              seq
              (fn [node children]
                (let [ret (cond
                            (vector? node) (vec children)
                            (map? node) (into {} children)
                            (seq? node) (seq children))]
                  (with-meta ret (meta node))))
              query))

;;;; Query types

(defn query-root? [qz]
  (and (vector? (zip/node qz))
       (loop [sub-qz (zip/down qz)]
         (when (query-expr? sub-qz)
           (if (zip/right sub-qz)
             (recur (zip/right sub-qz))
             true)))))

(defn ident-expr? [qz]
  (let [expr (zip/node qz)]
    (and (vector? expr)
         (= (count expr) 2)
         (keyword? (first expr)))))

(defn join-expr? [qz]
  (let [expr (zip/node qz)]
    (and (map? expr)
         (= (count expr) 1)
         (let [source-expr (zip/down (zip/down qz))]
           (or (keyword? (zip/node source-expr))
               (ident-expr? source-expr)))
         (let [query-expr (zip/right (zip/down (zip/down qz)))]
           (or (query-root? query-expr)
               ;; TODO: union-expr? + recur-expr?
               )))))

(defn plain-query-expr? [qz]
  (or (keyword? (zip/node qz))
      (ident-expr? qz)
      (join-expr? qz)))

(defn param-map-expr? [qz]
  (let [expr (zip/node qz)]
    (and (map? expr)
         (every? keyword? (keys expr)))))

(defn param-expr? [qz]
  (let [expr (zip/node qz)]
    (and (seq? expr)
         (= (count expr) 2)
         (plain-query-expr? (zip/down qz))
         (param-map-expr? (zip/right (zip/down qz))))))

(defn query-expr? [qz]
  (or (plain-query-expr? qz)
      (param-expr? qz)))

;;;; Query tree navigation

(defn parent-query [qz]
  (zip/up qz))

(defn first-subquery [qz]
  (when (query-root? qz)
    (zip/down qz)))

(defn next-query [qz]
  (zip/right qz))

(defn last-query? [qz]
  (nil? (zip/right qz)))

(defn join-source [qz]
  (if (join-expr? qz)
    (zip/down (zip/down qz))))

(defn join-query [qz]
  (if (join-expr? qz)
    (zip/right (zip/down (zip/down qz)))))

(defn param-query [qz]
  (if (param-expr? qz)
    (zip/down qz)))

(defn param-map [qz]
  (if (param-expr? qz)
    (zip/right (zip/down qz))))

;;;; Query tree processing

(defn process-keyword [qz ctx f ret]
  (println "KEYWORD")
  (f ret qz ctx))

(defn process-ident [qz ctx f ret]
  (println "IDENT")
  (f ret qz ctx))

(defn process-join [qz ctx f ret]
  (println "JOIN")
  (let [ret' (f ret qz ctx)]
    (cond
      (query-root? (join-query qz))
      (process-query-root (join-query qz) ctx f ret')

      ;; TODO: union-expr? + recur-expr?
      )))

(defn process-plain-query-expr [qz ctx f ret]
  (cond
    (keyword? (zip/node qz)) (process-keyword qz ctx f ret)
    (ident-expr? qz) (process-ident qz ctx f ret)
    (join-expr? qz) (process-join qz ctx f ret)))

(defn process-param-expr [qz _ f ret]
  (println "PARAM")
  (let [query (param-query qz)
        params (zip/node (param-map qz))]
    (process-plain-query-expr query {:params params} f ret)))

(defn process-query-expr [qz _ f ret]
  (println "QUERY")
  (cond
    (plain-query-expr? qz) (process-plain-query-expr qz nil f ret)
    (param-expr? qz) (process-param-expr qz nil f ret)))

(defn process-query-root [qz _ f ret]
  (println "QUERY ROOT")
  (loop [ret ret sub-qz (first-subquery qz)]
    (let [ret' (process-query-expr sub-qz nil f ret)]
      (if (last-query? sub-qz)
        ret'
        (recur ret' (next-query sub-qz))))))

(defn process [qz f ret]
  (when (query-root? qz)
    (process-query-root qz nil f ret)))
