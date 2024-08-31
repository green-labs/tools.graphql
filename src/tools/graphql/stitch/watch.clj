(ns tools.graphql.stitch.watch
  (:require [clojure.java.io :as io]
            [tools.graphql.stitch.core :as stitch]
            [tools.graphql.stitch.watcher :as watcher]))

(def path->schema (atom {}))

(defn- stitch-all
  [{:keys [output-file pretty]}]
  {:pre [(some? output-file)]}
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

(defn- create-subscriber [opts]
  (reify watcher/Subscription
    (-created [_ path]
      (println (format "Created %s" path))
      (assert (not (contains? @path->schema path)) "The schema already exists.")
      (swap! path->schema assoc path (stitch/read-subschema (io/file path) opts))
      (stitch-all opts))
    (-modified [_ path]
      (println (format "Modified %s" path))
      (assert (contains? @path->schema path) "The schema does not exist.")
      (swap! path->schema assoc path (stitch/read-subschema (io/file path) opts))
      (stitch-all opts))
    (-deleted [_ path]
      (println (format "Deleted %s" path))
      (assert (contains? @path->schema path) "The schema does not exist.")
      (swap! path->schema dissoc path)
      (stitch-all opts))))

(defn watch
  "Watch the given directories and stitch the schemas when the files are created, modified, or deleted.

  watch-opts:
    :path-to-root - the root path to watch (required)
    :dirs - the directories to watch (required)
    :output-file - the path to write the superschema (required)
    :pretty - pretty print the output
    :transformers - a list of functions to transform the schema before stitching"
  [& {:keys [path-to-root dirs] :as opts}]
  ;; check if path-to-root is a directory
  (assert (.isDirectory (io/file path-to-root)) "The path-to-root does not exist.")
  (prebuild-all dirs opts)
  (println "Watching" dirs)
  (watcher/subscribe dirs
                     path-to-root
                     (create-subscriber opts)))
