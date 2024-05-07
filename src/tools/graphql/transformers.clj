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
  ":Node 를 implements 하는 type인 경우 Edge, Connection 이 붙은 type 정보를 반환합니다.

  FIX: 모든 노드가 커넥션이 될 수 있다는 전제로 타입을 확장하고 있음. 정확히 커넥션이 필요한 경우에만 확장될 수 있도록 새로운 방법을 찾아야 함."
  [k v]
  (when (contains? (set (:implements v)) :Node)
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
                                :description "Number of edges"}}}])))

(defn extend-relay-types
  "lacinia schema 에서 정의한 objects 들에게
   Graphql relay spec 에 맞는 edges, connections 를 추가해줍니다."
  [schema]
  (update schema :objects
          #(reduce-kv (fn [m k v]
                        (apply assoc m
                               (concat [k v] (make-edge-connection-types k v))))
                      {} %)))

(comment
  (let [fields '{:id        {:type (! ID)},
                 :thisMonth {:type (! [(! :AttendanceDay)]), :description "이번달 일자별 출석 정보"},
                 :prevMonth {:type (! [(! :AttendanceDay)]), :description "지난달 일자별 출석 정보"},
                 :event     {:type (! [(! :AttendanceEvent)]), :description "진행 중인 보충 출석 이벤트"},
                 :ticket    {:type (! [(! :AttendanceTicket)]), :description "현재 유효한 보충 출석권들"}}]
    (transform-type-sugar fields)))