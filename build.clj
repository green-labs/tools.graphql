(ns build
  "For help, run:

  clojure -A:deps -T:build help/doc"
  (:refer-clojure :exclude [test])
  (:require
    [clojure.tools.build.api :as b]
    [deps-deploy.deps-deploy :as dd]))

(def lib 'org.clojars.greenlabs/tools.graphql)
(def version (format "1.0.%s" (b/git-count-revs nil)))
(def class-dir "target/classes")


(defn- pom-template
  [version]
  [[:description "Clojure tools for GraphQL"]
   [:url "https://github.com/green-labs/tools.graphql"]
   [:licenses
    [:license
     [:name "Apache License, Version 2.0"]
     [:url "http://www.apache.org/licenses/LICENSE-2.0"]]]
   [:scm
    [:url "https://github.com/green-labs/tools.graphql"]
    [:connection "scm:git:https://github.com/green-labs/tools.graphql.git"]
    [:developerConnection "scm:git:ssh:git@github.com:green-labs/tools.graphql.git"]
    [:tag (str "v" version)]]])

(def ^:private basis
  (delay (b/create-basis {})))

(defn- jar-opts
  [opts]
  (println "Version:" version)
  (assoc opts
    :lib lib   :version version
    :jar-file  (format "target/%s-%s.jar" lib version)
    :basis     @basis
    :class-dir class-dir
    :target    "target"
    :src-dirs  ["src"]
    :pom-data  (pom-template version)))

(defn clean
  [_]
  (println "Removing target/ ...")
  (b/delete {:path "target"}))

(defn build-jar
  [opts]
  (let [j-opts (jar-opts opts)]
    (println "Writing pom.xml ...")
    (b/write-pom j-opts)
    (println "Copying source ...")
    (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
    (println "Building" (:jar-file j-opts) "...")
    (b/jar j-opts)))

(defn uberjar
  [opts]
  (let [j-opts (assoc opts
                 :lib lib   :version version
                 :jar-file  (format "target/%s-cli-%s.jar" lib version)
                 :basis     (b/create-basis {:aliases [:cli]})
                 :class-dir class-dir
                 :target    "target"
                 :src-dirs  ["src"])]
    (clean nil)
    (b/copy-dir {:src-dirs ["src"] :target-dir class-dir})
    (b/compile-clj j-opts)
    (b/uber (merge j-opts {:uber-file (:jar-file j-opts)
                           :main      'tools.graphql.cli}))))

;; clj -T:build uberjar
;; native-image -jar target/org.clojars.greenlabs/tools.graphql-cli-1.0.47.jar --no-fallback --features=clj_easy.graal_build_time.InitClojureClasses tg

(defn deploy
  "Build the jar and deploy it to Clojars. Expects env vars CLOJARS_USERNAME &
  CLOJARS_PASSWORD."
  [opts]
  (clean opts)
  (build-jar opts)
  (let [{:keys [jar-file] :as opts} (jar-opts opts)]
    (dd/deploy {:installer :remote
                :artifact  (b/resolve-path jar-file)
                :pom-file  (b/pom-path (select-keys opts [:lib :class-dir]))}))
  opts)
