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

(comment
  ;; Processing

  (defn fetch-join-data [parent-data z ctx]
    (println "    FETCH JOIN DATA")
    (if-not parent-data
      (do
        ;; NO PARENT DATA
        ;;  (keyword? join-source) -> Fetch one or many
        ;;  (ident? join-source) -> Fetch one
        )
      (do
        ;; PARENT DATA
        ;;  (keyword? join-source)
        ;;    -> Extract values (IDs) of join source attrs in parent/parents
        ;;    -> Identify the target entity of the join source attr
        ;;    -> Fetch items of that entity with the IDs
        ;;    -> Spread the results across all parent entities,
        ;;       depending on which parent entity refered to which item
        ;;  (ident? join-source)
        ;;    -> Fetch data for the ident
        ;;    -> Insert it into all parents using the ident's name as the key
        )
      ))

  (defn fetch-data [parent-data z ctx]
    (println "  FETCH DATA FOR" (zip/node z))
    (println "            ctx" ctx)
    (println "    parent data" parent-data)
    (cond
      (qz/join-expr? z) (fetch-join-data parent-data z ctx)
      :else parent-data))

  (let [z (qz/query-zipper
           '[({:components
               [:db/id
                :component/name
                ({:component/states
                  [:db/id
                   :component-state/name
                   {:component-state/component [:component/name]}]}
                 {:sort/attr :component-state/name})]}
              {:sort/attr :component/name})])]
    (qz/process z fetch-data nil))


  )
