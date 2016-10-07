(ns workflo.query-engine.query.data-zip
  (:require [clojure.zip :as zip]))

;;;; Data zipper

(defn data-zipper
  ([]
   (data-zipper {}))
  ([data]
   (zip/zipper (some-fn vector? map? set?)
               seq
               (fn [node children]
                 (cond
                   (vector? node) (vec children)
                   (map? node) (into {} children)
                   (set? node) (into #{} children)))
               data)))

;;;; Data tree navigation and modification

(defn goto-attr
  "Moves the data zipper z to the value of the attribute k.
   Assumes that the z currently points to a map."
  [z k]
  {:pre [(map? (zip/node z))]}
  (loop [subz (zip/down z)]
    (if (= k (zip/node (zip/down subz)))
      (zip/right (zip/down subz))
      (if (zip/right subz)
        (recur (zip/right subz))
        nil))))

(defn goto-parent-map
  "Moves the data zipper to the map surrounding the current
   node of the zipper (e.g. an attribute name or value)."
  [z]
  (zip/up (zip/up z)))

(defn set-attr
  "Sets the value of the attribute k in the entity instance
   represented by the data zipper z to the value v."
  [z k v]
  {:pre [(map? (zip/node z))]}
  (zip/edit z assoc k v))

(defn entity-collection?
  "Returns true if the data zipper z currently points to a
   collection of entities, not to a single entity."
  [z]
  ((some-fn vector? set?) (zip/node z)))

(defn goto-parent-collection
  "Moves the data zipper from an entity instance to the
   collection it belongs to."
  [z]
  {:pre [(entity-collection? (zip/up z))]}
  (zip/up z))

(defn first-entity
  "Moves the zipper to the first entity in a collection."
  [z]
  {:pre [(entity-collection? z)]}
  (zip/down z))

(defn last-entity?
  "Returns true if the data zipper z points to the last entity
   in a collection of entities."
  [z]
  {:pre [(entity-collection? (zip/up z))]}
  (nil? (zip/right z)))

(defn next-entity
  "Moves the zipper to the next entity in the collection."
  [z]
  {:pre [(entity-collection? (zip/up z))]}
  (zip/right z))
