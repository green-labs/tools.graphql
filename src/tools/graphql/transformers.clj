(ns tools.graphql.transformers
  (:require [clojure.walk :as walk]))

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
  [schema]
  (walk/postwalk (fn [v]
                   (if (and (map-entry? v) (= :type (first v)))
                     (transform-!-and-vector v)
                     v))
                 schema))

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
  [schema]
  (update schema :objects
          #(reduce-kv (fn [m k v]
                        (if (= (:pagination v) :relay)
                          (as-> m $
                                (assoc $ k (dissoc v :pagination))
                                (apply assoc $ (make-edge-connection-types k)))
                          (assoc m k v)))
                      {} %)))

(comment
  (let [fields '{:id        {:type (! ID)},
                 :thisMonth {:type (! [(! :AttendanceDay)]), :description "이번달 일자별 출석 정보"},
                 :prevMonth {:type (! [(! :AttendanceDay)]), :description "지난달 일자별 출석 정보"},
                 :event     {:type (! [(! :AttendanceEvent)]), :description "진행 중인 보충 출석 이벤트"},
                 :ticket    {:type (! [(! :AttendanceTicket)]), :description "현재 유효한 보충 출석권들"}}]
    (transform-type-sugar fields))

  :rcf)