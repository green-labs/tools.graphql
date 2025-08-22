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

(defn- transform-simplified-directives
  "Transform simplified directive syntax to lacinia format
  
  Examples:
  {:auth {:roles [:ADMIN]}} -> [{:directive-type :auth :directive-args {:roles [:ADMIN]}}]
  {:tag [{:name \"sensitive\"} {:name \"pii\"}]} -> [{:directive-type :tag :directive-args {:name \"sensitive\"}}
                                                      {:directive-type :tag :directive-args {:name \"pii\"}}]
  "
  [directives directive-defs]
  (when (map? directives)
    (->> directives
         (mapcat (fn [[directive-type directive-value]]
                   (let [directive-def (get directive-defs directive-type)]
                     (cond
                       ;; List of maps for repeatable directive
                       (and (:repeatable directive-def) (sequential? directive-value))
                       (map (fn [value]
                              {:directive-type directive-type
                               :directive-args value})
                            directive-value)
                       
                       ;; List for non-repeatable directive (error)
                       (and (not (:repeatable directive-def)) (sequential? directive-value))
                       (throw (ex-info (format "Directive %s is not repeatable but received a list of values: %s" 
                                               directive-type directive-value)
                                       {:directive-type directive-type
                                        :repeatable false
                                        :values directive-value}))
                       
                       ;; Map value (args)
                       (map? directive-value)
                       [{:directive-type directive-type
                         :directive-args directive-value}]
                       
                       ;; Invalid value type
                       :else
                       (throw (ex-info (format "Invalid directive value for %s: %s. Expected map or list of maps for repeatable directives." 
                                               directive-type directive-value)
                                       {:directive-type directive-type
                                        :value directive-value}))))))
         vec)))

(defn transform-directives
  "Transform simplified directive syntax throughout the schema"
  [schema]
  (let [directive-defs (:directive-defs schema)]
    (walk/postwalk
     (fn [v]
       (cond
         ;; Handle :directives key with map value (simplified syntax)
         (and (map-entry? v) (= :directives (first v)) (map? (second v)))
         [:directives (transform-simplified-directives (second v) directive-defs)]
         
         ;; Leave existing lacinia format unchanged
         :else v))
     schema)))

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