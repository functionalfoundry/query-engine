(ns workflo.query-engine.query.zip
  (:require [clojure.zip :as zip]
            [workflo.macros.query.util :as query.util]
            [workflo.query-engine.query.data-zip :as dz]))

;;;; Forward declarations

(declare query-expr?)

;;;; Query zipper

(defn query-zipper
  "Creates a zipper for an Om Next query expression."
  [query]
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

(defn query-root?
  "Returns true if z is a query root expression."
  [z]
  (and (vector? (zip/node z))
       (loop [sub-z (zip/down z)]
         (when (query-expr? sub-z)
           (if (zip/right sub-z)
             (recur (zip/right sub-z))
             true)))))

(defn ident-expr?
  "Returns true if z is an ident expression."
  [z]
  (let [expr (zip/node z)]
    (and (vector? expr)
         (= (count expr) 2)
         (keyword? (first expr)))))

(defn join-expr?
  "Returns true if z is a join expression."
  [z]
  (let [expr (zip/node z)]
    (and (map? expr)
         (= (count expr) 1))))

(defn plain-query-expr?
  "Returns true if z is a plain query expression."
  [z]
  (or (keyword? (zip/node z))
      (ident-expr? z)
      (join-expr? z)))

(defn param-map-expr?
  "Returns true if z is a parameter map expression."
  [z]
  (let [expr (zip/node z)]
    (map? expr)))

(defn param-expr?
  "Returns true if z is a parameterized query expression."
  [z]
  (let [expr (zip/node z)]
    (and (seq? expr)
         (= (count expr) 2))))

(defn query-expr?
  "Returns true if z is a query expression."
  [z]
  (or (plain-query-expr? z)
      (param-expr? z)))

;;;; Query tree navigation

(defn parent-query
  "Moves to the parent query of the query expression."
  [z]
  (zip/up z))

(defn has-parent?
  "Returns true if the query z has a parent query."
  [z]
  (not (nil? (parent-query z))))

(defn toplevel?
  "Returns true if the query z is at the top level, meaning it is
   either an immediate child of the root query or the child of
   a topevel parameterized query."
  [z]
  (let [self? (not (has-parent? (parent-query z)))
        parent-param? (param-expr? (parent-query z))
        parent-toplevel? (not (-> z parent-query parent-query
                                  has-parent?))]
    (or self?
        (and parent-param?
             parent-toplevel?))))

(defn first-subquery
  "Moves to the first subquery of a query root z."
  [z]
  (when (query-root? z)
    (zip/down z)))

(defn next-query
  "Moves to the next query in a vector of query expressions."
  [z]
  (zip/right z))

(defn last-query?
  "Returns true if z is the last query expression in a vector
   of query expressions."
  [z]
  (nil? (zip/right z)))

(defn join-source
  "Moves to the source of a join expression."
  [z]
  (zip/down (zip/down z)))

(defn join-query
  "Moves to the query of a join expression."
  [z]
  (let [join-query (zip/right (zip/down (zip/down z)))]
    (cond-> join-query
      (= (zip/node join-query) '...)
      (-> parent-query parent-query parent-query))))

(defn param-query
  "Moves to the query of a param expression."
  [z]
  (zip/down z))

(defn param-map
  "Moves to the parameter map of a param expression."
  [z]
  (zip/right (zip/down z)))

(defn ident-name
  "Moves to the name of an ident expression."
  [z]
  (zip/down z))

(defn ident-value
  "Moves to the value of an ident expression."
  [z]
  (zip/right (zip/down z)))

(defn dispatch-key
  "Obtains the key to use for storing the results of processing
   the query z."
  [z]
  (cond
    (keyword? (zip/node z)) (zip/node z)
    (ident-expr? z) (dispatch-key (ident-name z))
    (join-expr? z) (dispatch-key (join-source z))
    (param-expr? z) (dispatch-key (param-query z))))

;;;; Query tree processing

(declare process-query-root)

(defn process-keyword
  "Processes a query keyword."
  [ret z params f]
  (let [ret' (f ret z params)]
    (if (toplevel? z)
      (zip/edit ret assoc (dispatch-key z) ret')
      (if-not (nil? ret')
        (zip/edit ret assoc (dispatch-key z) ret')
        ret))))

(defn process-ident
  "Processes an ident expression. This calls f to obtain a result for
   the ident and stores the result in ret under the dispatch key of
   the ident (e.g. `:account` for an ident like `[:account 1]`)."
  [ret z params f]
  (zip/edit ret assoc (dispatch-key z) (f ret z params)))

(defn process-join-query
  "Processes the query z of a join against one or more instances of
   an entity (in ret). Returns the result of proccessing the query
   for each of the instances. For instance, it may return a data
   zipper pointing to the input instance(s) again but with the
   extra data from processing the join query inserted into each
   instance."
  [ret z params f]
  (cond
    ;; Dealing with a collection of entities
    ;; -> process the join query for each entity instance
    (dz/entity-collection? ret)
    (if (seq (zip/node ret))
      (loop [child (dz/first-entity ret)]
        (let [child' (process-query-root child z nil f)]
          (if (dz/last-entity? child')
            (dz/goto-parent-collection child')
            (recur (dz/next-entity child')))))
      ret)

    ;; Dealing with just a single entity instance
    ;; -> process the join query for it
    (zip/node ret)
    (process-query-root ret z nil f)

    ;; There were not entities the join could be resolved to
    ;; -> return the data unmodified
    :else
    ret))

(defn process-join
  "Processes a join z with optional parameters against one or
   many instances of an entity (pointed to by the data zipper
   ret)."
  [ret z params f]
  (let [entity-or-entities (f (when-not (toplevel? z) ret) z params)
        singular-backref?  (let [key (dispatch-key z)]
                             (and (query.util/backref-attr? key)
                                  (query.util/singular-backref-attr? key)))]
    (if (some-> entity-or-entities meta :stop-processing?)
      (dz/set-attr ret (dispatch-key z)
                   (cond-> entity-or-entities
                     singular-backref? first))
      (let [meta-before (meta (zip/node ret))]
        (zip/edit (dz/goto-parent-map
                   (-> (dz/set-attr ret (dispatch-key z)
                                    (cond-> entity-or-entities
                                      singular-backref? first))
                       (dz/goto-attr (dispatch-key z))
                       (process-join-query (join-query z) nil f)))
                  with-meta meta-before)))))

(defn process-plain-query-expr
  "Processes a plain query expression."
  [ret z params f]
  (cond
    (keyword? (zip/node z)) (process-keyword ret z params f)
    (ident-expr? z) (process-ident ret z params f)
    (join-expr? z) (process-join ret z params f)))

(defn process-param-expr
  "Processes a parmeterized query expression by extracting
   the parameter map from it and passing them on to the
   processing of the contained plain query expression."
  [ret z _ f]
  (let [query (param-query z)
        params (zip/node (param-map z))]
    (process-plain-query-expr ret query params f)))

(defn process-query-expr
  "Processes a query expression (either plain or parameterized)."
  [ret z _ f]
  (cond
    (plain-query-expr? z) (process-plain-query-expr ret z nil f)
    (param-expr? z) (process-param-expr ret z nil f)))

(defn process-query-root
  "Processes a query root expression."
  [ret z _ f]
  (loop [ret ret sub-z (first-subquery z)]
    (let [ret' (process-query-expr ret sub-z nil f)]
      (cond-> ret'
        (not (last-query? sub-z)) (recur (next-query sub-z))))))

(defn process
  "Processes a query. Takes in a data zipper as the initial value
   and calls f for every keyword, join and ident expression in the
   query in order to refine the result at the given level of the
   query."
  [z f init]
  (when (query-root? z)
    (process-query-root init z nil f)))
