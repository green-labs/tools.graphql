(ns tools.graphql.stitch.watcher
  "Minimal watchman client for Clojure. Only supports subscribe."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]]
            [clojure.string :as str]
            [nextjournal.beholder :as beholder])
  (:import (java.io Reader Writer)
           (java.nio.channels Channels)
           (java.nio.file Path Paths)
           (org.newsclub.net.unix AFUNIXSocketAddress AFUNIXSocketChannel)))

(set! *warn-on-reflection* true)

(defprotocol Subscription
  (-created [this ^String path])
  (-modified [this ^String path])
  (-deleted [this ^String path]))

(defn- get-sockname []
  (-> (sh "watchman" "get-sockname")
      :out
      (json/read-str :key-fn keyword)
      :sockname))

(defn create-watchman-client []
  (let [sockname (get-sockname)
        channel  (-> sockname
                     io/file
                     AFUNIXSocketAddress/of
                     AFUNIXSocketChannel/open)]
    {:reader (-> channel
                 Channels/newInputStream
                 io/reader)
     :writer (-> channel
                 Channels/newOutputStream
                 io/writer)}))

(defn watchman
  "Subscribe to the given path with the given subscription name and args.
  The function f will be called with the message from the subscription."
  [{:keys [^Writer writer ^Reader reader]} path-to-root subscription-name args subscriber]
  (let [pdu (str (json/write-str ["subscribe" path-to-root subscription-name args]) \newline)]
    (println "Sending" pdu)
    (.write writer pdu)
    (.flush writer)
    (doseq [line (line-seq reader)
            :let [message (json/read-str line :key-fn keyword)]
            :when (not (:is_fresh_instance message))]
      (doseq [{:keys [name new exists]} (:files message)]
        (println (format "Received message: %s %s %s" name new exists))
        (if new
          (-created subscriber name)
          (if exists
            (-modified subscriber name)
            (-deleted subscriber name)))))))

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