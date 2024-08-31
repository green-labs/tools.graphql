(ns tools.graphql.stitch.watcher
  "Minimal watchman client for Clojure. Only supports subscribe."
  (:require [clojure.string :as str]
            [nextjournal.beholder :as beholder])
  (:import (java.nio.file Path Paths)))

(set! *warn-on-reflection* true)

(defprotocol Subscription
  (-created [this ^String path])
  (-modified [this ^String path])
  (-deleted [this ^String path]))

(defn- to-path ^Path [& args]
  (Paths/get ^String (first args) (into-array String (rest args))))

(defn subscribe
  [dirs path-to-root subscriber]
  (let [ptr (to-path path-to-root)
        cb  (fn [{:keys [type ^Path path] :as _result}]
              (when (str/ends-with? (str path) ".edn")
                (let [path (str (.relativize ptr path))]
                  (case type
                    :modify (-modified subscriber path)
                    :create (-created subscriber path)
                    :delete (-deleted subscriber path)))))]
    (apply beholder/watch-blocking cb dirs)))

(comment
  (def ptr (System/getProperty "user.dir"))

  (beholder/watch-blocking (fn [x] (println x)) "test-resources")

  (subscribe ["test-resources"]
             ptr
             (reify Subscription
               (-created [_ path]
                 (println "Created" path))
               (-modified [_ path]
                 (println "Modified" path))
               (-deleted [_ path]
                 (println "Deleted" path)))))