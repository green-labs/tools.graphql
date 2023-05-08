(ns user
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [farmmorning.core-api.graphql.schema :refer [schemas]]
            [farmmorning.graphql-helper.interface :refer [support-graphql-expression]]
            [gosura.helpers.relay :as ghr]
            [medley.core :refer [deep-merge]]
            [tools.graphql.sdl :refer [edn->sdl]])
  (:import (java.io PushbackReader Writer)))

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
       (support-graphql-expression)
       ghr/extend-relay-types))

(defn write-sdl [^Writer writer schema]
  (loop [data (edn->sdl schema)]
    (when-first [datum data]
      (.write writer ^String datum)
      (when-let [more (next data)]
        (.write writer "\n")
        (recur more)))))

(defn print-merged-schema
  [_args]
  (binding []
    (pprint/pprint merged-schema)))

(defn generate-sdl
  [{:keys [filename]}]
  (if filename
    (with-open [wrtr (clojure.java.io/writer (name filename))]
      (write-sdl wrtr merged-schema))
    (write-sdl *out* merged-schema)))

(comment

  (write-sdl *out* merged-schema)

  (generate-sdl {:filename "hello.gql"})

  :rcf)
