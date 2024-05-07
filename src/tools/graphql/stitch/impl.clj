(ns tools.graphql.stitch.impl
  "Lacinia 의 스키마를 분할하여 관리할 수 있도록 도와주는 API
  특정 경로 하위에 있는 모든 .edn 파일을 읽어서 스키마를 합친 뒤, output 에 해당하는 최종 스키마 파일을 생성합니다.

  the-guild.dev 에서 stitch 라고 부르는 개념입니다.
  https://the-guild.dev/graphql/stitching
  "
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.set :as set]
            [clojure.tools.logging :as log])
  (:import (java.io File PushbackReader)))

(set! *warn-on-reflection* true)

(defrecord Subschema [path contents])

(defn- validate-subschema!
  [path {:keys [queries mutations]}]
  (when (or (seq queries) (seq mutations))
    (log/warn ":queries and :mutations keys are deprecated." (str "(" path ")"))))

(defn read-edn
  "schema 스펙 확장을 지원하는 edn 리더"
  [path]
  (with-open [r (io/reader path)]
    (edn/read {:default tagged-literal}
              (PushbackReader. r))))

(defn- assoc-when
  "When the value is not nil, assoc the key-value pair."
  [m k v]
  (if v
    (assoc m k v)
    m))

(defn normalize-root-objects
  "Reserved names for top-level objects in Lacinia are Query, Mutation, Subscription.
  Bring them up to :queries, :mutations, :subscription."
  [schema]
  (-> schema
      (assoc-when :queries (get-in schema [:objects :Query :fields]))
      (assoc-when :mutations (get-in schema [:objects :Mutation :fields]))
      (assoc-when :subscriptions (get-in schema [:objects :Subscription :fields]))
      (update :objects dissoc :Query :Mutation :Subscription)))

(defn read-subschema
  "Read a subschema from the given path.
  TODO: validate if it's valid lacinia schema.

  args:
    path - path to read the subschema
    opts:
      :transformers - a list of functions to transform the schema before stitching."
  [^File path & {:keys [transformers]}]
  (let [contents     (read-edn path)
        transformers (conj (vec transformers) normalize-root-objects)

        ;; transform
        contents     (reduce (fn [schema f] (f schema)) contents transformers)

        ;; validate
        _            (validate-subschema! path contents)]
    (map->Subschema {:path     path
                     :name     (.getName path)
                     :contents contents})))

(defn read-subschemas
  "Read all subschemas in the given directory."
  [patterns & {:as opts}]
  (let [find-schemas (fn [pattern]
                       (->> (file-seq (io/file pattern))
                            (filter (fn [^File f] (-> f .getName (.endsWith ".edn"))))))]
    (->> patterns
         (mapcat find-schemas)
         (pmap #(read-subschema % opts)))))

(defn- intersecting-keys
  [a b]
  (set/intersection (set (keys a)) (set (keys b))))

(defn stitch-subschemas
  "Merge two subschemas into one subschema. If there is a conflict, throw an exception."
  [{a :contents} {b :contents :as subschema-b}]
  (let [res (merge-with merge a b)
        _   (when-not (= (merge-with + (update-vals a count) (update-vals b count))
                         (update-vals res count))
              (let [conflicts (->> (map (fn [[k v]]
                                          (intersecting-keys v (k b)))
                                        a)
                                   (filter seq))]
                (throw (ex-info (format "Conflict detected on %s" (:name subschema-b))
                                {:subschema subschema-b
                                 :conflicts conflicts}))))]
    (map->Subschema {:contents res})))

(defn print-schema
  [^Subschema schema & {:keys [pretty] :as _opts}]
  (if pretty
    (pprint/pprint (:contents schema))
    (pr (:contents schema))))

(comment

  (read-subschemas ["../../bases/core-api/resources/schema"
                    "../../bases/core-api/src"])

  (def subschemas
    (read-subschemas ["../../bases/core-api/resources/schema"]))

  (read-subschema (io/file "../../bases/core-api/resources/schema/user.edn"))

  (let [whole (time (reduce stitch-subschemas subschemas))]
    (time (print-schema whole :pretty false)))

  :rcf)
