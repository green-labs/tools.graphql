(ns tools.graphql.file
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [farmmorning.core-api.graphql.schema :refer [schemas]]
            [farmmorning.graphql-helper.interface :refer [support-graphql-expression]]
            [medley.core :refer [deep-merge]]
            [tools.graphql.sdl :refer [edn->sdl]])
  (:import (java.io PushbackReader)))

(defn- read-edn
  [path]
  (with-open [r (io/reader path)]
    (edn/read {:readers (merge default-data-readers
                               {'var identity})}
              (PushbackReader. r))))

(def merged-schema
  (->> schemas
       (map io/resource)
       (map read-edn)
       (apply deep-merge)
       (support-graphql-expression)))

(defn generate-sdl
  [{:keys [file-name]}]
  (with-open [wrtr (clojure.java.io/writer (name file-name))]
    (.write wrtr (s/join "\n" (edn->sdl merged-schema)))))

(comment

  (generate-sdl {:file-name "hello.gql"})

  :rcf)
