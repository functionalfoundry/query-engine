#!/usr/bin/env boot

(def +project+ 'workflo/query-engine)
(def +version+ "0.1.0-SNAPSHOT")

(def +repositories+
  [["workflo-private"
    {:url "https://workflo.jfrog.io/workflo/workflo-private"
     :username "boot"
     :password "[Kabc$?L~TMW"}]])

(set-env!
 :resource-paths #{"src/main" "resources"}
 :dependencies '[;; Boot
                 [adzerk/boot-test "1.1.2"]
                 [adzerk/bootlaces "0.1.13"]
                 [boot-environ "1.1.0"]
                 [boot-codox "0.10.0" :scope "test"]

                 ;; Core
                 [org.clojure/clojure "1.9.0-alpha11"]
                 [org.clojure/clojurescript "1.9.229"]

                 ;; General
                 [inflections "0.12.2"]
                 [environ "1.1.0"]
                 [workflo/macros "0.2.16"]

                 ;; Data layers
                 [com.datomic/datomic-free "0.9.5394" :scope "test"
                  :exclusions [com.google.guava/guava]]
                 [datomic-schema "1.3.0"]
                 [datascript "0.15.4" :scope "test"]])

(require '[adzerk.boot-test :refer :all]
         '[adzerk.bootlaces :refer :all]
         '[boot.git :refer [last-commit]]
         '[codox.boot :refer [codox]]
         '[environ.boot :refer [environ]])

(task-options!
 push {:repo "workflo-private"
       :ensure-branch "master"
       :ensure-clean true
       :ensure-tag (last-commit)
       :ensure-version +version+}
 pom {:project +project+
      :version +version+
      :description "Workflo query engine"
      :url "https://github.com/workfloapp/query-engine"
      :scm {:url "https://github.com/workfloapp/query-engine"}})

(deftask docs
  []
  (comp
   (codox :name "workflo/query-engine"
          :source-paths #{"src/main"}
          :output-path "docs"
          :metadata {:doc/format :markdown})
   (target)))

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

(deftask deploy-local
  []
  (comp
   (pom)
   (jar)
   (install)))

(deftask deploy-snapshot
  []
  (comp
   (pom)
   (jar)
   (build-jar)
   (push-snapshot)))

(deftask deploy-release
  []
  (comp
   (pom
    (jar)
    (build-jar)
    (push-release))))
