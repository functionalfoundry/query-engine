(ns workflo.query-engine.query.zip-test
  (:require [clojure.test :refer [are deftest is]]
            [workflo.query-engine.query.zip :as qz]
            [clojure.zip :as zip]))

;;;; Type checks

(deftest type-checks
  (and
   ;; Query root
   (let [z (qz/query-zipper [:a :b :c])]
     (and (is (qz/query-root? z))
          (is (not (qz/query-expr? z)))))

   ;; Keyword
   (let [z (qz/query-zipper :a)]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (qz/plain-query-expr? z))
          (is (keyword? (zip/node z)))))

   ;; Ident
   (let [z (qz/query-zipper '[:a _])]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (qz/plain-query-expr? z))
          (is (qz/ident-expr? z))))

   ;; Join with keyword source
   (let [z (qz/query-zipper {:a [:b :c]})]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (qz/plain-query-expr? z))
          (is (qz/join-expr? z))))

   ;; Join with ident source
   (let [z (qz/query-zipper '{[:a _] [:b :c]})]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (qz/plain-query-expr? z))
          (is (qz/join-expr? z))))

   ;; Param with a keyword query
   (let [z (qz/query-zipper '(:a {:b :c}))]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (not (qz/plain-query-expr? z)))
          (is (qz/param-expr? z))))

   ;; Param with a join query
   (let [z (qz/query-zipper '({:a [:b :c]} {:d :e}))]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (not (qz/plain-query-expr? z)))
          (is (qz/param-expr? z))))

   ;; Param with an ident query
   (let [z (qz/query-zipper '([:a _] {:b :c}))]
     (and (is (not (qz/query-root? z)))
          (is (qz/query-expr? z))
          (is (not (qz/plain-query-expr? z)))
          (is (qz/param-expr? z))))))

;;;; Navigating the query tree

(deftest parent-query
  (and
   ;; Query root
   (is (nil? (qz/parent-query (qz/query-zipper [:a :b :c]))))

   ;; Keyword
   (is (nil? (qz/parent-query (qz/query-zipper :a))))
   (is (= [:a :b :c] (-> [:a :b :c] qz/query-zipper
                         qz/first-subquery qz/parent-query
                         zip/node)))

   ;; Ident
   (is (nil? (qz/parent-query (qz/query-zipper '[:a _]))))
   (is (= '[:a [:b _] :c] (-> '[:a [:b _] :c] qz/query-zipper
                              qz/first-subquery qz/next-query
                              qz/parent-query zip/node)))

   ;; Join
   (is (nil? (qz/parent-query (qz/query-zipper '{:a [:b :c]}))))
   (is (= [:a {:b [:c :d]} :e] (-> [:a {:b [:c :d]} :e] qz/query-zipper
                                   qz/first-subquery qz/next-query
                                   qz/parent-query zip/node)))

   ;; Param
   (is (nil? (qz/parent-query (qz/query-zipper '(:a {:b :c})))))
   (is (= '[:a (:b {:c :d}) :e] (-> '[:a (:b {:c :d}) :e] qz/query-zipper
                                    qz/first-subquery qz/next-query
                                    qz/parent-query zip/node)))))

(deftest first-subquery-next-query-and-last-query?
  (let [z (qz/query-zipper '[:a {:b [:c]} (:d {:e :f})])]
    (and (is (= :a (-> z qz/first-subquery zip/node)))
         (is (= {:b [:c]} (-> z qz/first-subquery qz/next-query zip/node)))
         (is (= '(:d {:e :f}) (-> z qz/first-subquery qz/next-query
                                  qz/next-query zip/node)))
         (is (false? (-> z qz/first-subquery qz/last-query?)))
         (is (false? (-> z qz/first-subquery qz/next-query qz/last-query?)))
         (is (true? (-> z qz/first-subquery qz/next-query
                        qz/next-query qz/last-query?))))))

(deftest no-first-subquery-if-not-a-query-root-expr
  (and (is (nil? (-> :a qz/query-zipper qz/first-subquery)))
       (is (nil? (-> {:b [:c]} qz/query-zipper qz/first-subquery)))
       (is (nil? (-> '(:d {:e :f}) qz/query-zipper qz/first-subquery)))))

(deftest join-source-and-query
  (and
   ;; Join with a keyword source
   (let [z (qz/query-zipper {:a [:b :c]})]
     (and (is (= :a (zip/node (qz/join-source z))))
          (is (= [:b :c] (zip/node (qz/join-query z))))))

   ;; Join with an ident source
   (let [z (qz/query-zipper '{[:a _] [:b :c]})]
     (and (is (= '[:a _] (zip/node (qz/join-source z))))
          (is (= [:b :c] (zip/node (qz/join-query z))))))))

(deftest param-query-and-map
  (and
   ;; Param with a keyword query
   (let [z (qz/query-zipper '(:a {:b :c}))]
     (and (is (= :a (zip/node (qz/param-query z))))
          (is (= {:b :c} (zip/node (qz/param-map z))))))

   ;; Param with an ident query
   (let [z (qz/query-zipper '([:a _] {:b :c}))]
     (and (is (= '[:a _] (zip/node (qz/param-query z))))
          (is (= {:b :c} (zip/node (qz/param-map z))))))

   ;; Param with a join query
   (let [z (qz/query-zipper '({:a [:b :c]} {:d :e}))]
     (and (is (= '{:a [:b :c]} (zip/node (qz/param-query z))))
          (is (= {:d :e} (zip/node (qz/param-map z))))))))
