(ns workflo.query-engine.query.process
  (:require [workflo.macros.query.util :as query.util]
            [workflo.query-engine.query.data :as d]))

;;;; Forward declarations

(declare query-expr?)

;;;; Query types

(defn query-root?
  "Returns true if q is a query root expression."
  [q]
  (and (vector? q)
       (every? query-expr? q)))

(defn ident-expr?
  "Returns true if q is an ident expression."
  [q]
  (and (vector? q)
       (= (count q) 2)
       (keyword? (first q))))

(defn join-expr?
  "Returns true if q is a join expression."
  [q]
  (and (map? q)
       (= (count q) 1)))

(defn plain-query-expr?
  "Returns true if q is a plain query expression."
  [q]
  (or (keyword? q)
      (ident-expr? q)
      (join-expr? q)))

(def param-map-expr?
  "Returns true if q is a parameter map expression."
  map?)

(defn param-expr?
  "Returns true if q is a parameterized query expression."
  [q]
  (and (seq? q)
       (= (count q) 2)))

(defn query-expr?
  "Returns true if q is a query expression."
  [q]
  (or (plain-query-expr? q)
      (param-expr? q)))

;;;; Query tree navigation

(defn toplevel?
  "Returns true if the query q is at the top level, meaning it is
   either an immediate child of the root query or the child of
   a topevel parameterized query."
  [q parent-qs]
  (let [self? (empty? (drop 1 parent-qs))
        parent-param? (param-expr? (first parent-qs))
        parent-param-toplevel? (empty? (drop 2 parent-qs))]
    (or self?
        (and parent-param?
             parent-param-toplevel?))))

(def join-source
  "Returns the source of a join query expression."
  ffirst)

(defn join-query
  "Returns the query of a join query expression."
  [q parent-qs]
  (let [query (second (first q))]
    (if (= query '...)
      (first parent-qs)
      query)))

(def param-query
  "Returns the query of a parameterized query expression."
  first)

(def param-map
  "Returns the parameter map of a parameterized query expression."
  second)

(def ident-name
  "Returns the name of an ident expression."
  first)

(def ident-value
  "Returns the value of an ident expression."
  second)

(defn dispatch-key
  "Obtains the key to use for storing the results of processing
   the query q."
  [q]
  (cond
    (keyword? q) q
    (ident-expr? q) (dispatch-key (ident-name q))
    (join-expr? q) (dispatch-key (join-source q))
    (param-expr? q) (dispatch-key (param-query q))))

;;;; Query tree processing

(declare process-query-root)

(defn process-keyword
  "Processes a query keyword."
  [ret q parent-qs params f]
  (let [ret' (f ret q parent-qs params)]
    (if (toplevel? q parent-qs)
      (assoc ret (dispatch-key q) ret')
      (if-not (nil? ret')
        (assoc ret (dispatch-key q) ret')
        ret))))

(defn process-ident
  "Processes an ident expression. This calls f to obtain a result for
   the ident and stores the result in ret under the dispatch key of
   the ident (e.g. `:account` for an ident like `[:account 1]`)."
  [ret q parent-qs params f]
  (assoc ret (dispatch-key q) (f ret q parent-qs params)))

(defn process-join-query
  "Processes the query q of a join against one or more instances of
   an entity (in ret). Returns the result of proccessing the query
   for each of the instances. For instance, it may return the input
   instance(s) again but with the extra data from processing the
   join query inserted into each instance."
  [ret q parent-qs params f]
  (cond
    ;; Dealing with a collection of entities
    ;; -> process the join query for each entity instance
    (d/entity-collection? ret)
    (into (if (set? ret) #{} [])
          (map #(process-query-root % q parent-qs nil f))
          ret)

    ;; Dealing with just a single entity instance
    ;; -> process the join query for it
    ret (process-query-root ret q parent-qs nil f)

    ;; There were not entities the join could be resolved to
    ;; -> return the data unmodified
    :else ret))

(defn process-join
  "Processes a join q with optional parameters against one or
   many instances of an entity (in ret)."
  [ret q parent-qs params f]
  (let [entity-or-entities (f (when-not (toplevel? q parent-qs) ret)
                              q parent-qs params)
        singular-backref?  (let [key (dispatch-key q)]
                             (and (query.util/backref-attr? key)
                                  (query.util/singular-backref-attr? key)))]
    (if (some-> entity-or-entities meta :stop-processing?)
      (assoc ret (dispatch-key q) (cond-> entity-or-entities
                                    singular-backref? first))
      (assoc ret (dispatch-key q)
             (process-join-query (cond-> entity-or-entities
                                   singular-backref? first)
                                 (join-query q parent-qs)
                                 (cons q parent-qs)
                                 nil f)))))

(defn process-plain-query-expr
  "Processes a plain query expression."
  [ret q parent-qs params f]
  (cond
    (keyword? q) (process-keyword ret q parent-qs params f)
    (ident-expr? q) (process-ident ret q parent-qs params f)
    (join-expr? q) (process-join ret q parent-qs params f)))

(defn process-param-expr
  "Processes a parmeterized query expression by extracting
   the parameter map from it and passing them on to the
   processing of the contained plain query expression."
  [ret q parent-qs _ f]
  (let [query (param-query q)
        params (param-map q)]
    (process-plain-query-expr ret query (cons q parent-qs) params f)))

(defn process-query-expr
  "Processes a query expression (either plain or parameterized)."
  [ret q parent-qs _ f]
  (cond
    (plain-query-expr? q) (process-plain-query-expr ret q parent-qs nil f)
    (param-expr? q) (process-param-expr ret q parent-qs nil f)))

(defn process-query-root
  "Processes a query root expression."
  [ret q parent-qs _ f]
  (reduce (fn [ret sub-q]
            (process-query-expr ret sub-q (cons q parent-qs) nil f))
          ret q))

(defn process
  "Processes a query. Takes in the initial value to merge the result
   into and calls f for every keyword, join and ident expression in the
   query in order to refine the result at the given level of the
   query."
  [q parent-qs f init]
  (when (query-root? q)
    (process-query-root init q parent-qs nil f)))
