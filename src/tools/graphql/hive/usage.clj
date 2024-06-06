(ns tools.graphql.hive.usage
  "Malli data spec for Hive usage reporting.
  https://the-guild.dev/graphql/hive/docs/specs/usage-reports")

(def OperationMapRecord
  [:map
   [:operation :string]
   [:operationName {:optional true} :string]
   [:fields [:vector {:min 1} :string]]])

(def Execution
  [:map
   [:ok :boolean]
   [:duration :int]
   [:errorsTotal :int]])

(def Client
  [:map
   [:name :string]
   [:version :string]])

(def Metadata
  [:map
   [:client {:optional true} #'Client]])

(def RequestOperation
  [:map
   [:timestamp :int]
   [:operationMapKey :string]
   [:execution #'Execution]
   [:metadata {:optional true} #'Metadata]])

(def SubscriptionOperation
  [:map
   [:timestamp :int]
   [:operationMapKey :string]
   [:metadata {:optional true} #'Metadata]])

(def Report
  [:map
   [:size :int]
   [:map [:map-of :string #'OperationMapRecord]]
   [:operations {:optional true} [:vector #'RequestOperation]]
   [:subscriptionOperations {:optional true} [:vector #'SubscriptionOperation]]])

(comment
  (require '[malli.core :as m])
  (m/validate Client {:name "client1", :version "1.0.0"})
  (m/validate OperationMapRecord {:operation "op1", :fields ["field1"]})
  (m/validate Report {:size 10, :map {"key1" {:operation "op1", :fields ["field1"]}}})

  :rcf)

