(ns workflo.query-engine.test-data
  (:require [datomic.api :as d]))

;;;; Accounts

(def accounts
  [{:db/id -1
    :account/name "Company A"
    :account/users [-10 -11]
    :account/libraries [-100 -101]}
   {:db/id -2
    :account/name "Company B"
    :account/users [-12]
    :account/libraries [-102]}
   {:db/id -3
    :account/name "Company C"
    :account/users [-13]}])

;;;; Users

(def users
  [{:db/id -10
    :user/name "Joe"
    :user/email "joe@email.com"
    :user/password "abcdef"
    :user/account -1}
   {:db/id -11
    :user/name "Jeff"
    :user/email "jeff@email.com"
    :user/password "ghijkl"
    :user/account -1}
   {:db/id -12
    :user/name "Linda"
    :user/email "linda@email.com"
    :user/password "mnopqr"
    :user/account -2}
   {:db/id -13
    :user/name "Ada"
    :user/email "ada@email.com"
    :user/password "8dgfog"
    :user/account -3}
   {:db/id -14
    :user/name "Unknown"
    :user/email "unknown@email.com"
    :user/password "sad923"}])

;;;; Component libraries

(def component-libraries
  [{:db/id -100
    :component-library/name "Shop Components"
    :component-library/account -1
    :component-library/creator -10
    :component-library/components [-1000 -1001]}
   {:db/id -101
    :component-library/name "Social Network Components"
    :component-library/account -1
    :component-library/creator -10
    :component-library/components [-1002 -1003]}
   {:db/id -102
    :component-library/name "Event Site Components"
    :component-library/account -2
    :component-library/creator -11
    :component-library/components [-1004]
    :component-library/public? true}])

;;;; Components

(def components
  [{:db/id -1000
    :component/name "Shop Item"
    :component/account -1
    :component/creator -10
    :component/states [-10000 -10001]}
   {:db/id -1001
    :component/name "Cart Info"
    :component/account -1
    :component/creator -11}
   {:db/id -1002
    :component/name "Like Button"
    :component/account -1
    :component/creator -10
    :component/states [-10002 -10003]}
   {:db/id -1003
    :component/name "Dislike Button"
    :component/account -1
    :component/creator -11
    :component/public? true
    :component/states [-10004 -10005]}
   {:db/id -1004
    :component/name "Seat Picker"
    :component/account -2
    :component/creator -12}])

;;;; Component states

(def component-states
  [{:db/id -10000
    :component-state/name "Shop Item Regular"
    :component-state/component -1000}
   {:db/id -10001
    :component-state/name "Shop Item Selected"
    :component-state/component -1000}
   {:db/id -10002
    :component-state/name "Like Button Regular"
    :component-state/component -1002}
   {:db/id -10003
    :component-state/name "Like Button Active"
    :component-state/component -1002}
   {:db/id -10004
    :component-state/name "Dislike Button Regular"
    :component-state/component -1003}
   {:db/id -10005
    :component-state/name "Dislike Button Active"
    :component-state/component -1003}])

;;;; Component trees

(def component-trees
  [{:db/id -20000
    :component-tree/root -20100}])

(def tree-components
  [{:db/id -20100
    :tree-component/component -1000
    :tree-component/children [-20101 -20102]}
   {:db/id -20101
    :tree-component/component -1002
    :tree-component/children [-20103]}
   {:db/id -20102
    :tree-component/component -1003
    :tree-component/children []}
   {:db/id -20103
    :tree-component/component -1004
    :tree-component/children []}])

;;;; All test data

(def combined
  (concat accounts
          users
          component-libraries
          components
          component-states
          component-trees
          tree-components))

;;;; Datomic test data

(def datomic
  (letfn [(mktempid [id]
            (d/tempid :db.part/user id))
          (mktempids [ids]
            (map mktempid ids))]
    (concat (map (fn [account]
                   (-> account
                       (update :db/id mktempid)
                       (update :account/users mktempids)
                       (update :account/libraries mktempids)))
                 accounts)
            (map (fn [user]
                   (cond-> user
                     true
                     (update :db/id mktempid)

                     (:user/account user)
                     (update :user/account mktempid)))
                 users)
            (map (fn [library]
                   (-> library
                       (update :db/id mktempid)
                       (update :component-library/account mktempid)
                       (update :component-library/creator mktempid)
                       (update :component-library/components mktempids)))
                 component-libraries)
            (map (fn [component]
                   (-> component
                       (update :db/id mktempid)
                       (update :component/account mktempid)
                       (update :component/creator mktempid)
                       (update :component/states mktempids)))
                 components)
            (map (fn [state]
                   (-> state
                       (update :db/id mktempid)
                       (update :component-state/component mktempid)))
                 component-states)
            (map (fn [tree]
                   (-> tree
                       (update :db/id mktempid)
                       (update :component-tree/root mktempid)))
                 component-trees)
            (map (fn [tree-component]
                   (-> tree-component
                       (update :db/id mktempid)
                       (update :tree-component/component mktempid)
                       (update :tree-component/children mktempids)))
                 tree-components))))
