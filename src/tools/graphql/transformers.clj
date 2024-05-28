(ns tools.graphql.transformers
  (:require [clojure.walk :as walk]
            [meander.epsilon :as m])
  (:import (tools.graphql.stitch.core Subschema)))

(defn- transform-!-and-vector
  [type]
  (walk/postwalk (fn [v]
                   (cond
                     ;; Replace '!' with 'non-null'
                     (and (symbol? v) (= v (symbol "!")))
                     (symbol "non-null")

                     ;; Change '[...]' to '(list ...)'
                     (and (not (map-entry? v))
                          (vector? v))
                     (list (symbol "list") (first v))

                     :else v))
                 type))

(defn transform-type-sugar
  "expand lacinia edn schema to support ! and []

  - ! to non-null
  - [...] to (list ...)
  "
  [^Subschema schema]
  (update schema :contents
          (fn [contents]
            (walk/postwalk (fn [v]
                             (if (and (map-entry? v) (= :type (first v)))
                               (transform-!-and-vector v)
                               v))
                           contents))))

(defn- make-edge-connection-types
  [k]
  (let [type-name       (name k)
        edge-name       (keyword (str type-name "Edge"))
        connection-name (keyword (str type-name "Connection"))]
    [edge-name
     {:implements [:Edge]
      :fields     {:cursor {:type        '(non-null String)
                            :description "Cursor"}
                   :node   {:type        `(~'non-null ~k)
                            :description "Node"}}}
     connection-name
     {:implements [:Connection]
      :fields     {:edges    {:type        `(~'non-null (~'list (~'non-null ~edge-name)))
                              :description "Edges"}
                   :pageInfo {:type        '(non-null :PageInfo)
                              :description "Page information"}
                   :count    {:type        '(non-null Int)
                              :description "Number of edges"}}}]))

(defn relay-pagination
  ":pagination key 가 :relay 인 type 들에게 Graphql relay spec 에 맞는 edges, connections 를 추가해줍니다.
   커넥션 스펙은 https://relay.dev/graphql/connections.htm 참고"
  [^Subschema schema]
  (update schema :contents
          (fn [contents]
            (update contents :objects
                    #(reduce-kv (fn [m k v]
                                  (if (= (:pagination v) :relay)
                                    (as-> m $
                                          (assoc $ k (dissoc v :pagination))
                                          (apply assoc $ (make-edge-connection-types k)))
                                    (assoc m k v)))
                                {} %)))))

(defn source-map
  [^Subschema schema]
  (let [embed-loc #(update-vals % (fn [v]
                                    (let [loc (assoc (meta v)
                                                :name (:name schema))]
                                      (assoc v :loc loc))))]
    (update schema :contents
            (fn [contents]
              (update-vals contents embed-loc)))))


(comment
  (let [fields '{:id        {:type (! ID)},
                 :thisMonth {:type (! [(! :AttendanceDay)]), :description "이번달 일자별 출석 정보"},
                 :prevMonth {:type (! [(! :AttendanceDay)]), :description "지난달 일자별 출석 정보"},
                 :event     {:type (! [(! :AttendanceEvent)]), :description "진행 중인 보충 출석 이벤트"},
                 :ticket    {:type (! [(! :AttendanceTicket)]), :description "현재 유효한 보충 출석권들"}}]
    (transform-type-sugar fields))

  (require '[tools.graphql.stitch.core :refer [read-subschema]])
  (require '[clojure.java.io :as io])

  (let [schema (read-subschema (io/file "test-resources/object.edn")
                               :transformers [source-map]
                               :source-map? true)]
    schema)

  :rcf)