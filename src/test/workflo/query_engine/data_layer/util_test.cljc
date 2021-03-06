(ns workflo.query-engine.data-layer.util-test
  (:require [clojure.test :refer [are deftest testing]]
            [workflo.query-engine.data-layer.util :as util]))

(deftest matches-params-rules
  (testing "matches-params? rule from simple parameters"
    (are [out in] (= out (util/matches-params-rules in))
      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)]
        [(matches-param-1? ?e)
         [?e :foo/bar "baz"]]]
      {:foo/bar "baz"}

      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)]
        [(matches-param-1? ?e)
         [?e :bar/baz 5]]]
      {:bar/baz 5}

      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)
         (matches-param-2? ?e)]
        [(matches-param-1? ?e)
         [?e :foo/bar "baz"]]
        [(matches-param-2? ?e)
         [?e :bar/baz 5]]]
      {:foo/bar "baz"
       :bar/baz 5}))

  (testing "matches-params? rule from deep parameters"
    (are [out in] (= out (util/matches-params-rules in))
      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)]
        [(matches-param-1? ?e)
         [?e :foo/bar ?var-1]
         [?var-1 :bar/baz "baz"]]]
      {[:foo/bar :bar/baz] "baz"})))

(deftest matches-params-rules-with-backrefs
  (testing "matches-params? rule can handle backrefs"
    (are [out in] (= out (util/matches-params-rules in))
      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)]
        [(matches-param-1? ?e)
         ["foo" :account/users ?e]]]
      {:account/_users "foo"}

      '[[(matches-params? ?e)
         [?e]
         (matches-param-1? ?e)]
        [(matches-param-1? ?e)
         [?e :permission-assignment/role ?var-1]
         [?var-2 :user/roles ?var-1]
         [?var-2 :workflo/id "1234"]]]
      {[:permission-assignment/role :user/_roles :workflo/id] "1234"})))
