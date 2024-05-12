(ns tools.graphql.stitch.watchman
  "Minimal watchman client for Clojure. Only supports subscribe."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.java.shell :refer [sh]])
  (:import (java.io Reader Writer)
           (java.nio.channels Channels)
           (org.newsclub.net.unix AFUNIXSocketAddress AFUNIXSocketChannel)))

(set! *warn-on-reflection* true)

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

(defprotocol Subscription
  (-created [this path])
  (-modified [this path])
  (-deleted [this path]))

(defn subscribe
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
