(ns workflo.query-engine.query.data-zip-test
  (:require [clojure.test :refer [are deftest is]]
            [workflo.query-engine.query.data-zip :as dz]
            [clojure.zip :as zip]))

(deftest goto-attr
  (let [z (dz/data-zipper {:foo :bar :baz :ruux})]
    (is (= :bar (-> z (dz/goto-attr :foo) zip/node)))
    (is (= :ruux (-> z (dz/goto-attr :baz) zip/node)))))

(deftest goto-parent-map
  (let [m {:foo :bar :baz :ruux}
        z (dz/data-zipper m)]
    (is (= m (-> z (dz/goto-attr :foo) dz/goto-parent-map zip/node)))
    (is (= m (-> z (dz/goto-attr :baz) dz/goto-parent-map zip/node)))))

(deftest set-attr
  (let [m {:foo :bar :baz :ruux}
        z (dz/data-zipper m)]
    (is (= {:foo :FOO :baz :ruux} (-> z
                                      (dz/set-attr :foo :FOO)
                                      zip/node)))
    (is (= {:foo :bar :baz :BAZ} (-> z
                                     (dz/set-attr :baz :BAZ)
                                     zip/node)))
    (is (= {:foo :FOO :baz :BAZ} (-> z
                                     (dz/set-attr :foo :FOO)
                                     (dz/set-attr :baz :BAZ)
                                     zip/node)))))

(deftest entity-collection?
  (is (true? (-> [{:db/id 1} {:db/id 2}]
                 dz/data-zipper
                 dz/entity-collection?)))
  (is (true? (-> #{{:db/id 1} {:db/id 2}}
                 dz/data-zipper
                 dz/entity-collection?)))
  (is (false? (-> {:db/id 1}
                  dz/data-zipper
                  dz/entity-collection?))))

(deftest first-entity-and-next-entity
  (let [c [{:db/id 1} {:db/id 2} {:db/id 3}]
        z (dz/data-zipper c)]
    (is (= {:db/id 1} (-> z dz/first-entity zip/node)))
    (is (= {:db/id 2} (-> z dz/first-entity dz/next-entity
                          zip/node)))
    (is (= {:db/id 3} (-> z dz/first-entity dz/next-entity
                          dz/next-entity zip/node)))
    (is (= nil (-> z dz/first-entity dz/next-entity
                   dz/next-entity dz/next-entity)))))

(deftest last-entity?
  (let [c [{:db/id 1} {:db/id 2} {:db/id 3}]
        z (dz/data-zipper c)]
    (is (false? (-> z dz/first-entity dz/last-entity?)))
    (is (false? (-> z dz/first-entity dz/next-entity
                    dz/last-entity?)))
    (is (true? (-> z dz/first-entity dz/next-entity
                   dz/next-entity dz/last-entity?)))))

(deftest goto-parent-collection
  (let [c [{:db/id 1} {:db/id 2}]
        z (dz/data-zipper c)]
    (is (= c (-> z dz/first-entity
                 dz/goto-parent-collection
                 zip/node)))
    (is (= c (-> z dz/first-entity dz/next-entity
                 dz/goto-parent-collection
                 zip/node)))))
