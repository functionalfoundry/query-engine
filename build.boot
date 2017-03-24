#!/usr/bin/env boot

(def +project+ 'workflo/query-engine)
(def +version+ "0.1.20")

(def +repositories+
  [["workflo-private"
    {:url "https://workflo.jfrog.io/workflo/workflo-private"
     :username (System/getenv "WORKFLO_REPOSITORIES_USERNAME")
     :password (System/getenv "WORKFLO_REPOSITORIES_PASSWORD")}]])

(set-env!
 :resource-paths #{"src/main"}
 :repositories #(concat % +repositories+)
 :dependencies '[;; Boot
                 [adzerk/boot-cljs "2.0.0" :scope "test"]
                 [adzerk/boot-test "1.2.0" :scope "test"]
                 [boot-environ "1.1.0" :scope "test"]
                 [boot-codox "0.10.3" :scope "test"]

                 ;; Core
                 [org.clojure/clojure "1.9.0-alpha15"]
                 [org.clojure/clojurescript "1.9.495"]

                 ;; General
                 [inflections "0.13.0"]
                 [environ "1.1.0"]

                 ;; Data layers
                 [com.datomic/datomic-free "0.9.5561"
                  :exclusions [com.google.guava/guava]
                  :scope "test"]
                 [datomic-schema "1.3.0" :scope "test"]
                 [datascript "0.15.5" :scope "test"]

                 ;; Workflo
                 [workflo/boot-tasks "0.1.9" :scope "test"]
                 [workflo/macros "0.2.46"]])

(require '[adzerk.boot-test :as boot-test]
         '[environ.boot :refer [environ]]
         '[workflo.boot-tasks :refer :all])

(workflo-setup! :project +project+
                :version +version+
                :library true
                :push-repo "workflo-private")

(task-options!
 pom {:description "Workflo query engine"
      :url "https://github.com/workfloapp/query-engine"
      :scm {:url "https://github.com/workfloapp/query-engine"}})

(deftask dev-env
  []
  (environ :env {:datomic-uri "datomic:mem://workflo-query-engine"}))

(deftask test-env
  []
  (comp (dev-env)
        (with-pre-wrap fileset
          (merge-env! :source-paths #{"src/test"})
          fileset)))

(deftask test
  []
  (comp (test-env)
        (boot-test/test)))

(deftask dev
  []
  (comp (dev-env)
        (repl)))
