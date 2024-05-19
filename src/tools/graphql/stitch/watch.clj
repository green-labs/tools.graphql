(ns tools.graphql.stitch.watch
  (:require [clojure.java.io :as io]
            [tools.graphql.stitch.core :as stitch]
            [tools.graphql.stitch.watchman :as watchman]))

(def path->schema (atom {}))

(defn- stitch-all
  [{:keys [output-file pretty]}]
  (let [schemas     (vals @path->schema)
        superschema (reduce stitch/stitch-subschemas schemas)]
    (time
     (spit output-file (with-out-str (stitch/print-schema superschema :pretty pretty))))))

(defn- prebuild-all
  [dirs opts]
  (let [schemas (stitch/read-subschemas dirs opts)
        paths   (map #(-> % :path str) schemas)]
    (reset! path->schema (zipmap paths schemas))
    (stitch-all opts)))

(defn- create-watcher [opts]
  (reify watchman/Subscription
    (-created [_ path]
      (assert (not (contains? @path->schema path)) "The schema already exists.")
      (swap! path->schema assoc path (stitch/read-subschema (io/file path) opts))
      (stitch-all opts))
    (-modified [_ path]
      (assert (contains? @path->schema path) "The schema does not exist.")
      (swap! path->schema assoc path (stitch/read-subschema (io/file path) opts))
      (stitch-all opts))
    (-deleted [_ path]
      (assert (contains? @path->schema path) "The schema does not exist.")
      (swap! path->schema dissoc path)
      (stitch-all opts))))

(defn watch
  "Watch the given directories and stitch the schemas when the files are created, modified, or deleted.

  watch-opts:
    :path-to-root - the root path to watch
    :dirs - the directories to watch
    :output-file - the path to write the superschema
    :pretty - pretty print the output
    :transformers - a list of functions to transform the schema before stitching"
  [& {:keys [path-to-root dirs] :as opts}]
  (prebuild-all dirs opts)
  (watchman/subscribe (watchman/create-watchman-client)
                      path-to-root
                      "tools.graphql.stitch.watch"
                      {:expression ["allof"
                                    ["dirname" (first dirs)]
                                    ["suffix" "edn"]]}
                      (create-watcher opts)))


(comment

  (def dirs ["../../bases/core-api/resources/schema/review"])
  (def opts {:transformers []})
  (def watcher (create-watcher opts))

  (watch {:path-to-root "/Users/namenu/green/farmmorning-clj"
          :dirs         ["bases/core-api/resources/schema"]})

  (watchman/-created watcher "../../bases/core-api/resources/schema/user.edn")
  (watchman/-modified watcher "../../bases/core-api/resources/schema/user.edn")
  (watchman/-deleted watcher "../../bases/core-api/resources/schema/user.edn")

  (count @path->schema)

  :rcf)
