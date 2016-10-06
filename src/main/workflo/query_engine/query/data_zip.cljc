(ns workflo.query-engine.query.data-zip
  (:require [clojure.zip :as zip]))

;;;; Data zipper

(defn data-zipper []
  (zip/zipper (some-fn vector? map? set?)
              seq
              (fn [node children]
                (cond
                  (vector? node) (vec children)
                  (map? node) (into {} children)
                  (set? node) (into #{} children)))
              {}))
