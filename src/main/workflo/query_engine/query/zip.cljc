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
