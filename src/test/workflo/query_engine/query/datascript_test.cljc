(ns workflo.query-engine.query.datascript-test
  (:require [clojure.test :refer [deftest use-fixtures]]
            [workflo.query-engine.query :as q]
            [workflo.query-engine.test-datascript :refer [setup]]
            [workflo.query-engine.query.common-test :as c]))

(deftest process-queries-without-cache
  (c/test-process-queries (assoc setup :cache? false)))

(deftest process-queries-with-cache
  (c/test-process-queries (assoc setup :cache? true)))
