(ns workflo.query-engine.query.process-test
  (:require [clojure.test :refer [are deftest is]]
            [workflo.query-engine.query.process :as p]))

;;;; Type checks

(deftest type-checks
  ;; Query root
  (let [q [:a :b :c]]
    (is (p/query-root? q))
    (is (not (p/query-expr? q))))

  ;; Keyword
  (let [q :a]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (p/plain-query-expr? q))
    (is (keyword? q)))

  ;; Ident
  (let [q '[:a _]]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (p/plain-query-expr? q))
    (is (p/ident-expr? q)))

  ;; Join with keyword source
  (let [q {:a [:b :c]}]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (p/plain-query-expr? q))
    (is (p/join-expr? q)))

  ;; Join with ident source
  (let [q '{[:a _] [:b :c]}]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (p/plain-query-expr? q))
    (is (p/join-expr? q)))

  ;; Param with a keyword query
  (let [q '(:a {:b :c})]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (not (p/plain-query-expr? q)))
    (is (p/param-expr? q)))

  ;; Param with a join query
  (let [q '({:a [:b :c]} {:d :e})]
    (is (not (p/query-root? q)))
    (is (p/query-expr? q))
    (is (not (p/plain-query-expr? q)))
    (is (p/param-expr? q))))

;; Param with an ident query
(let [q '([:a _] {:b :c})]
  (and (is (not (p/query-root? q)))
       (is (p/query-expr? q))
       (is (not (p/plain-query-expr? q)))
       (is (p/param-expr? q))))

;;;; Navigating the query tree

(deftest join-source-and-query
  ;; Join with a keyword source
  (let [q {:a [:b :c]}]
    (is (= :a (p/join-source q)))
    (is (= [:b :c] (p/join-query q nil))))

  ;; Join with an ident source
  (let [q '{[:a _] [:b :c]}]
    (is (= '[:a _] (p/join-source q)))
    (is (= [:b :c] (p/join-query q nil)))))

(deftest param-query-and-map
  ;; Param with a keyword query
  (let [q '(:a {:b :c})]
    (is (= :a (p/param-query q)))
    (is (= {:b :c} (p/param-map q))))

  ;; Param with an ident query
  (let [q '([:a _] {:b :c})]
    (is (= '[:a _] (p/param-query q)))
    (is (= {:b :c} (p/param-map q))))

  ;; Param with a join query
  (let [q '({:a [:b :c]} {:d :e})]
    (is (= '{:a [:b :c]} (p/param-query q)))
    (is (= {:d :e} (p/param-map q)))))
