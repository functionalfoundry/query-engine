(ns workflo.query-engine.query.data
  (:require [workflo.macros.entity :as entities]))

(defn entity-instance? [x]
  (and (map? x)
       (or (contains? x :db/id)
           (contains? x :workflo/id))))

(defn entity-collection? [x]
  (or (vector? x)
      (set? x)))
