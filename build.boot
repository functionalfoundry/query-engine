#!/usr/bin/env boot

(def +project+ 'workflo/query-engine)
(def +version+ "0.1.0-SNAPSHOT")

(def +repositories+
  [["workflo-private"
    {:url "https://workflo.jfrog.io/workflo/workflo-private"
     :username (System/getenv "WORKFLO_REPOSITORIES_USERNAME")
     :password (System/getenv "WORKFLO_REPOSITORIES_PASSWORD")}]])

(set-env!
 :resource-paths #{"src/main" "resources"}
 :repositories #(concat % +repositories+)
 :dependencies '[;; Boot
                 [adzerk/boot-test "1.1.2"]
                 [boot-environ "1.1.0"]
                 [boot-codox "0.10.0" :scope "test"]

                 ;; Core
                 [org.clojure/clojure "1.9.0-alpha11"]
                 [org.clojure/clojurescript "1.9.229"]

                 ;; General
                 [inflections "0.12.2"]
                 [environ "1.1.0"]

                 ;; Data layers
                 [com.datomic/datomic-free "0.9.5394"
                  :scope "test"
                  :exclusions [com.google.guava/guava]]
                 [datomic-schema "1.3.0"]
                 [datascript "0.15.4" :scope "test"]

                 ;; Workflo
                 [workflo/boot-workflo "0.1.0-SNAPSHOT"]
                 [workflo/macros "0.2.16"]])

(require '[adzerk.boot-test :refer :all]
         '[codox.boot :refer [codox]]
         '[environ.boot :refer [environ]]
         '[workflo.boot-workflo :refer :all])

(workflo-setup!
 :project +project+
 :version +version+
 :module? false
 :uberjar? false
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
  (comp
   (environ :env {:datomic-uri "datomic:mem://workflo-query-engine"})
   (with-pre-wrap fileset
     (merge-env! :source-paths #{"src/test"})
     fileset)))

(deftask dev
  []
  (comp (dev-env)
        (repl)))
