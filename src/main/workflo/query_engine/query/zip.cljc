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

(defn query-root? [z]
  (and (vector? (zip/node z))
       (loop [sub-z (zip/down z)]
         (when (query-expr? sub-z)
           (if (zip/right sub-z)
             (recur (zip/right sub-z))
             true)))))

(defn ident-expr? [z]
  (let [expr (zip/node z)]
    (and (vector? expr)
         (= (count expr) 2)
         (keyword? (first expr)))))

(defn join-expr? [z]
  (let [expr (zip/node z)]
    (and (map? expr)
         (= (count expr) 1)
         (let [source-expr (zip/down (zip/down z))]
           (or (keyword? (zip/node source-expr))
               (ident-expr? source-expr)))
         (let [query-expr (zip/right (zip/down (zip/down z)))]
           (or (query-root? query-expr)
               ;; TODO: union-expr? + recur-expr?
               )))))

(defn plain-query-expr? [z]
  (or (keyword? (zip/node z))
      (ident-expr? z)
      (join-expr? z)))

(defn param-map-expr? [z]
  (let [expr (zip/node z)]
    (and (map? expr)
         (every? keyword? (keys expr)))))

(defn param-expr? [z]
  (let [expr (zip/node z)]
    (and (seq? expr)
         (= (count expr) 2)
         (plain-query-expr? (zip/down z))
         (param-map-expr? (zip/right (zip/down z))))))

(defn query-expr? [z]
  (or (plain-query-expr? z)
      (param-expr? z)))

;;;; Query tree navigation

(defn parent-query [z]
  (zip/up z))

(defn toplevel? [z]
  (let [self? (nil? (parent-query (parent-query z)))
        parent-param? (param-expr? (parent-query z))
        parent-toplevel? (nil? (parent-query (parent-query (parent-query z))))]
    (or self?
        (and parent-param?
             parent-toplevel?))))

(defn first-subquery [z]
  (when (query-root? z)
    (zip/down z)))

(defn next-query [z]
  (zip/right z))

(defn last-query? [z]
  (nil? (zip/right z)))

(defn join-source [z]
  (if (join-expr? z)
    (zip/down (zip/down z))))

(defn join-query [z]
  (if (join-expr? z)
    (zip/right (zip/down (zip/down z)))))

(defn param-query [z]
  (if (param-expr? z)
    (zip/down z)))

(defn param-map [z]
  (if (param-expr? z)
    (zip/right (zip/down z))))

(defn ident-name [z]
  (if (ident-expr? z)
    (zip/down z)))

(defn ident-value [z]
  (if (ident-expr? z)
    (zip/right (zip/down z))))

(defn query-key [z]
  (cond
    (keyword? (zip/node z)) (zip/node z)
    (ident-expr? z) (query-key (ident-name z))
    (join-expr? z) (query-key (join-source z))
    (param-expr? z) (query-key (param-query z))))

;;;; Data tree navigation

(defn focus-map-key
  [z key]
  (loop [subz (zip/down z)]
    (if (= key (zip/node (zip/down subz)))
      (zip/right (zip/down subz))
      (if (zip/right subz)
        (recur (zip/right subz))
        nil))))

(defn unfocus-map-key
  [z]
  (zip/up (zip/up z)))

;;;; Query tree processing

(declare process-query-root)

(defn process-keyword [z params f ret]
  (cond-> ret
    (toplevel? z) (zip/edit assoc (query-key z) (f nil z params))))

(defn process-ident [z params f ret]
  (zip/edit ret assoc (query-key z) (f ret z params)))

(defn process-join-query [z params f ret]
  (let [entity-or-entities (zip/node ret)]
    (if (or (nil? entity-or-entities)
            (empty? entity-or-entities))
      ret
      (if ((some-fn vector? set?) entity-or-entities)
        (if (seq entity-or-entities)
          (loop [child (zip/down ret)]
            (let [child' (process-query-root z nil f child)]
              (if-not (zip/right child')
                (zip/up child')
                (recur (zip/right child')))))
          ret)
        (process-query-root z nil f ret)))))

(defn process-join [z params f ret]
  (if (toplevel? z)
    (let [entity-or-entities (f nil z params)]
      (unfocus-map-key
       (let [data' (-> (zip/edit ret assoc (query-key z)
                                 entity-or-entities)
                       (focus-map-key (query-key z)))]
         (process-join-query (join-query z) nil f data'))))
    (let [entity-or-entities (f ret z params)]
      (unfocus-map-key
       (let [data' (-> (zip/edit ret assoc (query-key z)
                                 entity-or-entities)
                       (focus-map-key (query-key z)))]
         (process-join-query (join-query z) nil f data'))))))

(defn process-plain-query-expr [z params f ret]
  (cond
    (keyword? (zip/node z)) (process-keyword z params f ret)
    (ident-expr? z) (process-ident z params f ret)
    (join-expr? z) (process-join z params f ret)))

(defn process-param-expr [z _ f ret]
  (let [query (param-query z)
        params (zip/node (param-map z))]
    (process-plain-query-expr query params f ret)))

(defn process-query-expr [z _ f ret]
  (cond
    (plain-query-expr? z) (process-plain-query-expr z nil f ret)
    (param-expr? z) (process-param-expr z nil f ret)))

(defn process-query-root [z _ f ret]
  (loop [ret ret sub-z (first-subquery z)]
    (let [ret' (process-query-expr sub-z nil f ret)]
      (println "QUERY EXPR" (zip/node sub-z))
      (println "  <-" (zip/node ret))
      (println "  ->" (zip/node ret'))
      (if (last-query? sub-z)
        ret'
        (recur ret' (next-query sub-z))))))

(defn process [z f init]
  (println "---")
  (when (query-root? z)
    (process-query-root z nil f init)))
