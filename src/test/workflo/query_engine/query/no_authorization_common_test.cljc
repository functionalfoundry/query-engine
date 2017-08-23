(ns workflo.query-engine.query.no-authorization-common-test
  (:require [clojure.test :refer [are]]
            [workflo.macros.entity :as e]
            [workflo.macros.query.om-next :as query.om-next]
            [workflo.query-engine.core :as qe]))

(defn test-process-queries
  [{:keys [connect db db-config data-layer transact resolve-tempid
           new-cache cache? id-attr ref-id-attr]}]
  (println (if cache?
             "Test processing queries with cache"
             "Test processing queries without cache"))
  (let [conn (connect)
        layer (data-layer)
        tx (transact conn)
        resolve-id (partial resolve-tempid conn tx)
        shared-cache (new-cache)]
    (are [args result]
        (= result
           (let [{:keys [query empty-cache? query-hooks]} args]
             (qe/query query layer
                       {:db (db conn)
                        :db-config db-config
                        :id-attr id-attr
                        :ref-id-attr ref-id-attr
                        :cache (if empty-cache?
                                 (new-cache)
                                 shared-cache)}
                       {:query-hooks query-hooks})))

      ;; Query components via a top-level keyword; all components are
      ;; returned with their `id-attr` values, as no attributes are
      ;; specified in the query
      {:query [:components]}

      {:components #{{id-attr (resolve-id -1000)}
                     {id-attr (resolve-id -1001)}
                     {id-attr (resolve-id -1002)}
                     {id-attr (resolve-id -1003)}
                     {id-attr (resolve-id -1004)}}}

      ;; Query a specific component via a ident; it is returned
      ;; only with its `id-attr` value, since no attributes are
      ;; specified in the query
      {:query [[:component (resolve-id -1000)]]}

      {:component {id-attr (resolve-id -1000)}}

      ;; Query an account with no library references and verify
      ;; that an empty set of references is returned rather than
      ;; nil
      {:query [{[:account (resolve-id -3)]
                [:account/name
                 {:account/users [:db/id]}
                 {:account/libraries [:db/id]}]}]}

      {:account {:account/name "Company C"
                 :account/users #{{:db/id (resolve-id -13)}}
                 :account/libraries #{}}}

      ;; Query for components with a keyword join
      {:query [{:components [:component/name]}]}

      {:components
       #{{:component/name "Shop Item"}
         {:component/name "Cart Info"}
         {:component/name "Like Button"}
         {:component/name "Dislike Button"}
         {:component/name "Seat Picker"}}}

      ;; Query all accounts (implictly including their `id-attr` attribute)
      ;; and all components with their names
      {:query [:accounts {:components [:component/name]}]}

      {:accounts #{{id-attr (resolve-id -1)}
                   {id-attr (resolve-id -2)}
                   {id-attr (resolve-id -3)}}
       :components
       #{{:component/name "Shop Item"}
         {:component/name "Cart Info"}
         {:component/name "Like Button"}
         {:component/name "Dislike Button"}
         {:component/name "Seat Picker"}}}

      ;; Query for a specific component with a ident join
      {:query [{[:component (resolve-id -1000)]
                [:db/id :component/name :component/account]}]}

      {:component {:db/id (resolve-id -1000)
                   :component/name "Shop Item"
                   :component/account {ref-id-attr (resolve-id -1)}}}

      ;; Query all components and sort by their names using
      ;; a parameterized query
      {:query '[({:components [:component/name]}
                 {:sort/attr :component/name})]}

      {:components
       [{:component/name "Cart Info"}
        {:component/name "Dislike Button"}
        {:component/name "Like Button"}
        {:component/name "Seat Picker"}
        {:component/name "Shop Item"}]}

      ;; Query all components and sort in reverse order
      ;; by their names using a parameterized query
      {:query '[({:components [:component/name]}
                 {:sort/attr :component/name
                  :sort/order :sort/descending})]}

      {:components
       [{:component/name "Shop Item"}
        {:component/name "Seat Picker"}
        {:component/name "Like Button"}
        {:component/name "Dislike Button"}
        {:component/name "Cart Info"}]}

      ;; Query for one component and filter by :db/id
      {:query `[(:component {:db/id ~(resolve-id -1000)})]}

      {:component {id-attr (resolve-id -1000)}}

      ;; Query for one component and filter by :workflo/id
      {:query `[(:component {:workflo/id ~(resolve-id -1000)})]}

      {:component {id-attr (resolve-id -1000)}}

      ;; Query for all component and filter by :db/id
      {:query `[(:components {:db/id ~(resolve-id -1000)})]}

      {:components #{{id-attr (resolve-id -1000)}}}

      ;; Query for all components and parameterize the join
      ;; with their component states to sort their names
      ;; descendingly
      {:query '[{:components
                 [:component/name
                  ({:component/states [:component-state/name]}
                   {:sort/attr :component-state/name
                    :sort/order :sort/descending})]}]}

      {:components
       #{{:component/name "Dislike Button"
          :component/states
          [{:component-state/name "Dislike Button Regular"}
           {:component-state/name "Dislike Button Active"}]}
         {:component/name "Cart Info"
          :component/states #{}}
         {:component/name "Like Button"
          :component/states
          [{:component-state/name "Like Button Regular"}
           {:component-state/name "Like Button Active"}]}
         {:component/name "Seat Picker"
          :component/states #{}}
         {:component/name "Shop Item"
          :component/states
          [{:component-state/name "Shop Item Selected"}
           {:component-state/name "Shop Item Regular"}]}}}

      ;; Query for all components and parameterize the join
      ;; with their component states to sort their names
      ;; descendingly, this time also sort the components
      {:query '[({:components
                  [:component/name
                   ({:component/states [:component-state/name]}
                    {:sort/attr :component-state/name
                     :sort/order :sort/descending})]}
                 {:sort/attr :component/name
                  :sort/order :sort/descending})]}

      {:components
       [{:component/name "Shop Item"
         :component/states
         [{:component-state/name "Shop Item Selected"}
          {:component-state/name "Shop Item Regular"}]}
        {:component/name "Seat Picker"
         :component/states #{}}
        {:component/name "Like Button"
         :component/states
         [{:component-state/name "Like Button Regular"}
          {:component-state/name "Like Button Active"}]}
        {:component/name "Dislike Button"
         :component/states
         [{:component-state/name "Dislike Button Regular"}
          {:component-state/name "Dislike Button Active"}]}
        {:component/name "Cart Info"
         :component/states #{}}]}

      ;; Query one component by ID, join in its component states
      ;; and their component (which should be the same component
      ;; again)
      {:query `[({:component [:component/name
                              {:component/states
                               [:component-state/name
                                {:component-state/component
                                 [:component/name]}]}]}
                 {:db/id ~(resolve-id -1000)})]}

      {:component
       {:component/name "Shop Item"
        :component/states
        #{{:component-state/name "Shop Item Regular"
           :component-state/component
           {:component/name "Shop Item"}}
          {:component-state/name "Shop Item Selected"
           :component-state/component
           {:component/name "Shop Item"}}}}}

      ;; Query all components plus a specific account and user in each
      ;; component query via an ident
      {:query [{:components [:db/id
                             [:account (resolve-id -1)]
                             [:user (resolve-id -10)]]}]}

      {:components
       #{{:db/id (resolve-id -1000)
          :account {ref-id-attr (resolve-id -1)}
          :user {ref-id-attr (resolve-id -10)}}
         {:db/id (resolve-id -1001)
          :account {ref-id-attr (resolve-id -1)}
          :user {ref-id-attr (resolve-id -10)}}
         {:db/id (resolve-id -1002)
          :account {ref-id-attr (resolve-id -1)}
          :user {ref-id-attr (resolve-id -10)}}
         {:db/id (resolve-id -1003)
          :account {ref-id-attr (resolve-id -1)}
          :user {ref-id-attr (resolve-id -10)}}
         {:db/id (resolve-id -1004)
          :account {ref-id-attr (resolve-id -1)}
          :user {ref-id-attr (resolve-id -10)}}}}

      ;; Query a specific component plus a specific account's
      ;; information via ident joins
      {:query [{[:component (resolve-id -1000)]
                [:component/name {[:account (resolve-id -1)]
                                  [:account/name :account/users]}]}]}

      {:component
       {:component/name "Shop Item"
        :account {:account/name "Company A"
                  :account/users [{ref-id-attr (resolve-id -10)}
                                  {ref-id-attr (resolve-id -11)}]}}}

      ;; Query the component that contains a specific
      ;; component state
      {:query `[({:component [:component/name :component/states]}
                 {:component/states ~(resolve-id -10004)})]}

      {:component
       {:component/name "Dislike Button"
        :component/states [{ref-id-attr (resolve-id -10004)}
                           {ref-id-attr (resolve-id -10005)}]}}

      ;; Query all components that were created by a specific user
      {:query `[({:components [:component/name :component/creator]}
                 {:component/creator ~(resolve-id -10)})]}

      {:components
       #{{:component/name "Shop Item"
          :component/creator {ref-id-attr (resolve-id -10)}}
         {:component/name "Like Button"
          :component/creator {ref-id-attr (resolve-id -10)}}}}

      ;; Query a key that directly corresponds to a query hook
      {:query [:foo]
       :query-hooks {:foo (fn [env parent q parent-qs params] :bar)}}

      {:foo :bar}

      ;; Query using a top-level join that directly corresponds to
      ;; a query hook
      {:query [{:foo [:bar :baz]}]
       :query-hooks {:foo (fn [env parent q parent-qs params]
                            (zipmap q (map name q)))}}

      {:foo {:bar "bar" :baz "baz"}}

      ;; Query all components with their name and a non-existent
      ;; attribute that triggers a query hook
      {:query [{:components [:component/name :extra-info]}]
       :query-hooks {:extra-info (fn [env parent q parent-qs params] :foo)}}

      {:components
       #{{:component/name "Cart Info", :extra-info :foo}
         {:component/name "Dislike Button" :extra-info :foo}
         {:component/name "Like Button" :extra-info :foo}
         {:component/name "Seat Picker" :extra-info :foo}
         {:component/name "Shop Item" :extra-info :foo}}}

      ;; Query all components with their name and a join via
      ;; a non-existent attribute that triggers a query hook
      ;; to include arbitrary data in the result
      {:query [{:components [:component/name
                             {:extra-info [:a :b]}]}]
       :query-hooks {:extra-info (fn [env parent q parent-qs params]
                                   {:a :foo :b :bar})}}

      {:components
       #{{:component/name "Cart Info" :extra-info {:a :foo :b :bar}}
         {:component/name "Dislike Button" :extra-info {:a :foo :b :bar}}
         {:component/name "Like Button" :extra-info {:a :foo :b :bar}}
         {:component/name "Seat Picker" :extra-info {:a :foo :b :bar}}
         {:component/name "Shop Item" :extra-info {:a :foo :b :bar}}}}

      ;; Query components with a join that triggers a hook; verify
      ;; that if the hook sets the :stop-processing? meta flag, the
      ;; query isn't processed beyond the hook
      {:query [{:components [:component/name
                             {:extra [{:foo :bar}
                                      {:bar :baz}]}]}]
       :query-hooks {:extra (fn [env parent q parent-qs params]
                              (with-meta
                                {:a :b}
                                {:stop-processing? true}))}}

      {:components
       #{{:component/name "Cart Info" :extra {:a :b}}
         {:component/name "Dislike Button" :extra {:a :b}}
         {:component/name "Like Button" :extra {:a :b}}
         {:component/name "Seat Picker" :extra {:a :b}}
         {:component/name "Shop Item" :extra {:a :b}}}}

      ;; Query the accounts of users via a singular backref; due to
      ;; the singular backref and there being only a single account
      ;; in the :account/_users result, the data in :account/_users
      ;; becomes a single entity instead of a set
      {:query [{:users [id-attr :user/name
                        {:account/_users [:account/name
                                          {:account/users [:user/name]}]}]}]}
      {:users #{{id-attr (resolve-id -10)
                 :user/name "Joe"
                 :account/_users {:account/name "Company A"
                                  :account/users #{{:user/name "Joe"}
                                                   {:user/name "Jeff"}}}}
                {id-attr (resolve-id -11)
                 :user/name "Jeff"
                 :account/_users {:account/name "Company A"
                                  :account/users #{{:user/name "Joe"}
                                                   {:user/name "Jeff"}}}}
                {id-attr (resolve-id -12)
                 :user/name "Linda"
                 :account/_users {:account/name "Company B"
                                  :account/users #{{:user/name "Linda"}}}}
                {id-attr (resolve-id -13)
                 :user/name "Ada"
                 :account/_users {:account/name "Company C"
                                  :account/users #{{:user/name "Ada"}}}}
                {id-attr (resolve-id -14)
                 :user/name "Unknown"
                 :account/_users nil}}}

      ;; Query the accounts of users via a plural backref; due to
      ;; the plural backref, the result of :account/_users is turned
      ;; into a set, despite there being only a single account for each
      ;; user
      {:query [{:users [id-attr :user/name
                        {:accounts/_users [:account/name
                                           {:account/users [:user/name]}]}]}]}
      {:users #{{id-attr (resolve-id -10)
                 :user/name "Joe"
                 :accounts/_users #{{:account/name "Company A"
                                     :account/users #{{:user/name "Joe"}
                                                      {:user/name "Jeff"}}}}}
                {id-attr (resolve-id -11)
                 :user/name "Jeff"
                 :accounts/_users #{{:account/name "Company A"
                                     :account/users #{{:user/name "Joe"}
                                                      {:user/name "Jeff"}}}}}
                {id-attr (resolve-id -12)
                 :user/name "Linda"
                 :accounts/_users #{{:account/name "Company B"
                                     :account/users #{{:user/name "Linda"}}}}}
                {id-attr (resolve-id -13)
                 :user/name "Ada"
                 :accounts/_users #{{:account/name "Company C"
                                     :account/users #{{:user/name "Ada"}}}}}
                {id-attr (resolve-id -14)
                 :user/name "Unknown"
                 :accounts/_users #{}}}}

      ;; Query the states of components via a singular backref;
      ;; due to this, only the first state is returned in the
      ;; :component-state/_component attribute
      {:query `[{:components
                 [~id-attr
                  :component/name
                  ({:component-state/_component [:component-state/name]}
                   {:sort/attr :component-state/name})]}]}
      {:components
       #{{id-attr (resolve-id -1000)
          :component/name "Shop Item"
          :component-state/_component
          {:component-state/name "Shop Item Regular"}}
         {id-attr (resolve-id -1001)
          :component/name "Cart Info"
          :component-state/_component nil}
         {id-attr (resolve-id -1002)
          :component/name "Like Button"
          :component-state/_component
          {:component-state/name "Like Button Active"}}
         {id-attr (resolve-id -1003)
          :component/name "Dislike Button"
          :component-state/_component
          {:component-state/name "Dislike Button Active"}}
         {id-attr (resolve-id -1004)
          :component/name "Seat Picker"
          :component-state/_component nil}}}

      ;; Query the the states of components via a pluralized backref;
      ;; This results in all states of a component to be returned as
      ;; a set, under the pluralized attribute name
      {:query [{:components
                [id-attr
                 :component/name
                 {:component-states/_component
                  [:component-state/name]}]}]}
      {:components
       #{{id-attr (resolve-id -1000)
          :component/name "Shop Item"
          :component-states/_component
          #{{:component-state/name "Shop Item Regular"}
            {:component-state/name "Shop Item Selected"}}}
         {id-attr (resolve-id -1001)
          :component/name "Cart Info"
          :component-states/_component #{}}
         {id-attr (resolve-id -1002)
          :component/name "Like Button"
          :component-states/_component
          #{{:component-state/name "Like Button Regular"}
            {:component-state/name "Like Button Active"}}}
         {id-attr (resolve-id -1003)
          :component/name "Dislike Button"
          :component-states/_component
          #{{:component-state/name "Dislike Button Regular"}
            {:component-state/name "Dislike Button Active"}}}
         {id-attr (resolve-id -1004)
          :component/name "Seat Picker"
          :component-states/_component #{}}}}

      ;; Query with two joins at the same level
      {:query [{:accounts [:account/name
                           {:account/users [:user/name
                                            {:user/account [:account/name]}]}
                           {:account/libraries [:component-library/name]}]}]}
      {:accounts
       #{{:account/name "Company A"
          :account/users
          #{{:user/name "Joe"
             :user/account {:account/name "Company A"}}
            {:user/name "Jeff"
             :user/account {:account/name "Company A"}}}
          :account/libraries
          #{{:component-library/name "Shop Components"}
            {:component-library/name "Social Network Components"}}}
         {:account/name "Company B"
          :account/users
          #{{:user/name "Linda"
             :user/account {:account/name "Company B"}}}
          :account/libraries
          #{{:component-library/name "Event Site Components"}}}
         {:account/name "Company C"
          :account/users
          #{{:user/name "Ada"
             :user/account {:account/name "Company C"}}}
          :account/libraries #{}}}}

      ;; Query a user that has no account; with the join on the unset
      ;; :user/account attribute, the resulting account should be nil,
      ;; not #{} or {}
      {:query `[({:user [:user/name {:user/account [:db/id]}]}
                 {:db/id ~(resolve-id -14)})]}
      {:user {:user/name "Unknown"
              :user/account nil}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given :db/id
      {:query `[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :db/id] ~(resolve-id -1)})]}
      {:users
       #{{:user/name "Joe" :user/account {:account/name "Company A"}}
         {:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"})]}
      {:users
       #{{:user/name "Joe" :user/account {:account/name "Company A"}}
         {:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name and that have the name "Jeff"
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"
                  :user/name "Jeff"})]}
      {:users
       #{{:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use parameterization using a backref to achieve the same as above
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:account/_users :account/name] "Company A"
                  :user/name "Jeff"})]}

      {:users
       #{{:user/name "Jeff" :user/account {:account/name "Company A"}}}}

      ;; Use deep parameterization to query all users that belong
      ;; to the account with the given name and that have the name "Linda"
      {:query '[({:users [:user/name {:user/account [:account/name]}]}
                 {[:user/account :account/name] "Company A"
                  :user/name "Linda"})]}
      {:users #{}}

      ;; Use a recursive query to obtain all components in a component tree
      {:query `[({:component-tree
                  [{:component-tree/root
                    [:db/id
                     {:tree-component/component [:component/name]}
                     {:tree-component/children ...}]}]}
                 {:db/id ~(resolve-id -20000)})]}
      {:component-tree
       {:component-tree/root
        {:db/id (resolve-id -20100)
         :tree-component/component {:component/name "Shop Item"}
         :tree-component/children
         #{{:db/id (resolve-id -20101)
            :tree-component/component {:component/name "Like Button"}
            :tree-component/children
            #{{:db/id (resolve-id -20103)
               :tree-component/component {:component/name "Seat Picker"}
               :tree-component/children #{}}}}
           {:db/id (resolve-id -20102)
            :tree-component/component {:component/name "Dislike Button"}
            :tree-component/children #{}}}}}})))
