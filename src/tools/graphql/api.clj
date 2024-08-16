(ns tools.graphql.api
  (:require [clj-commons.ansi :refer [pcompose]]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [tools.graphql.sdl :as sdl]
            [tools.graphql.stitch.core :as stitch]
            [tools.graphql.stitch.watch :as stitch.watch]
            [tools.graphql.validators :as validators])
  (:import (java.io PushbackReader Writer)))

(defn- is-path-under-dir? [file-path dir-path]
  (let [file (-> file-path io/file .toPath)
        dir  (-> dir-path io/file .toPath)]
    (.startsWith file dir)))

(defn- coerce-to-file
  [path default-filename]
  (let [file (io/file path)]
    (if (.isDirectory file)
      (str (io/file path default-filename))
      path)))

(defn stitch-schemas
  "Read all subschemas in the given directory and merge them into one (superschema).

  args:
    :input-paths  - list of paths to read subschemas
    :output-path  - path to write the final schema (optional)
    :transformers - a list of functions to transform the schema before stitching (optional)
    :pretty       - pretty print the output (optional)
    :watch        - watch the input paths and stitch the schemas when subschemas are changed (optional)
    :source-map?  - inject source map to the schema (optional)
  "
  [& {:keys [input-paths output-path transformers source-map? pretty watch]}]
  (assert (sequential? input-paths))

  (let [subschemas  (stitch/read-subschemas input-paths {:transformers transformers
                                                         :source-map?  source-map?})
        _           (when-not (seq subschemas)
                      (throw (ex-info "No subschemas found" {})))
        superschema (reduce stitch/stitch-subschemas subschemas)]
    (if output-path
      (let [output-file (coerce-to-file output-path "superschema.edn")]
        (when (some #(is-path-under-dir? output-path %) input-paths)
          (throw (ex-info "output-file cannot located under input-path" {})))
        ;; must remove target file first
        (io/delete-file output-file :silent true)
        (if watch
          (stitch.watch/watch {:path-to-root (System/getProperty "user.dir")
                               :dirs         input-paths
                               :output-file  output-file
                               :pretty       pretty
                               :transformers transformers
                               :source-map?  source-map?})
          (spit output-file (with-out-str (stitch/print-schema superschema :pretty pretty)))))
      (:contents superschema))))

(defn- write-sdl [^Writer writer schema]
  (loop [data (sdl/edn->sdl schema)]
    (when-first [datum data]
      (.write writer ^String datum)
      (when-let [more (next data)]
        (.write writer "\n")
        (recur more)))))

(defn edn2sdl
  "Convert an edn file to a SDL file.

  args:
    :input-path - path to read the edn file
    :output-path - path to write the SDL file. If not provided, write to stdout. (optional)"
  [& {:keys [input-path output-path]}]
  (assert (string? input-path))

  (let [in     (io/file input-path)
        _      (when-not (.canRead in)
                 (throw (ex-info "Cannot read input file" {})))
        schema (stitch/read-edn in)]
    (if-let [out (io/file output-path)]
      (with-open [w (io/writer out)]
        (write-sdl w schema))
      (write-sdl *out* schema))))

(defn scan-vars
  [path]
  (let [vs (transient [])]
    (with-open [r (io/reader path)]
      (edn/read {:readers {'var #(conj! vs %)}}
                (PushbackReader. r))
      (persistent! vs))))

(defn scan-namespaces
  "Scan all namespaces in the given edn file.
  Use when AOT compile is needed.

  Args:
    :input-path - path to read the edn file"
  [& {:keys [input-path]}]
  (->> (scan-vars input-path)
       (map (comp symbol namespace))
       (distinct)))

(defn validate
  "Validate the edn GraphQL schema.

  args:
    :input-path - path to read the schema"
  [& {:keys [input-path]}]
  (let [schema    (stitch/read-edn (io/file input-path))
        print-loc (fn [loc]
                    (when loc
                      [:blue " " (:path loc) ":" (:row loc) ":" (:col loc)]))]
    (doseq [[t loc] (validators/unreachable-types schema)]
      (pcompose [:red "Unreachable type"] " " (name t) (print-loc loc)))
    (doseq [[f loc] (validators/unreachable-input-types schema)]
      (pcompose [:red "Unreachable input"] " " (name f) (print-loc loc)))
    (doseq [[i loc] (validators/unreachable-interfaces schema)]
      (pcompose [:red "Unreachable interface"] " " (name i) (print-loc loc)))
    (doseq [[i _loc field] (validators/interface-with-resolver schema)]
      (pcompose [:red "Interface should not have resolvers"] " " (name i) "." (name field)))
    (doseq [m (validators/relay-arguments schema)]
      (pcompose [:red "Invalid pagination arguments"] " " (name (:query m)) [:blue " " (:hint m)]))))
