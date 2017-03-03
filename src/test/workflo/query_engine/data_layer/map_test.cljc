(ns workflo.query-engine.data-layer.map-test
  (:require [clojure.test :refer [deftest]]
            [workflo.query-engine.test-map :refer [setup]]
            [workflo.query-engine.data-layer.no-authorization-common-test :as c]))

;;;; Fetch individual entities

(deftest fetch-one-without-cache
  (c/test-fetch-one (assoc setup :cache? false)))

(deftest fetch-one-with-cache
  (c/test-fetch-one (assoc setup :cache? true)))

;;;; Fetch a collection of entities

(deftest fetch-many-without-cache
  (c/test-fetch-many (assoc setup :cache? false)))

(deftest fetch-many-with-cache
  (c/test-fetch-many (assoc setup :cache? true)))

;;;; Fetch all instances of an entity

(deftest fetch-all-without-cache
  (c/test-fetch-all (assoc setup :cache? false)))

(deftest fetch-all-with-cache
  (c/test-fetch-all (assoc setup :cache? true)))

