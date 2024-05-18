(ns tools.graphql.validators
  (:require [meander.epsilon :as m]))

(defn unreachable-types [schema]
  (let [types        (m/search schema
                               {:objects {?type _}}
                               ?type)
        unreachable? (fn [t]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {:objects {?type {:fields {?field {:type ?t}}}}}
                                      :type   ?t}
                                     ?field
                                     ;; TODO: input-objects, enums, interfaces, unions
                                     {:schema {:queries {?query {:type ?t}}}
                                      :type   ?t}
                                     ?query
                                     {:schema {:mutations {?mutations {:type ?t}}}
                                      :type   ?t}
                                     ?mutations)))]
    {:unreachable-types (filter unreachable? types)}))

(defn unreachable-input-types [schema]
  (let [types        (m/search schema
                               {:input-objects {?type _}}
                               ?type)
        unreachable? (fn [t]
                       (nil? (m/find {:schema schema
                                      :type   t}
                                     {:schema {:query {?query {:args {?input {:type ?t}}}}}
                                      :type   ?t}
                                     ?input
                                     {:schema {:mutations {?mutations {:args {?input {:type ?t}}}}}
                                      :type   ?t}
                                     ?input)))]
    {:unreachable-input-types (filter unreachable? types)}))



(comment

  (def schema {:input-objects {:UserInput {:fields {:name     {:type 'String}
                                                    :email    {:type 'String}
                                                    :password {:type 'String}}}
                               :Dummy     {:fields {:id {:type 'String}}}}
               :objects       {:User {:fields {:id        {:type 'String}
                                               :name      {:type 'String}
                                               :email     {:type 'String}
                                               :password  {:type 'String}
                                               :createdAt {:type 'String}
                                               :updatedAt {:type 'String}
                                               :deletedAt {:type :Date}}}}
               :queries       {:user  {:type :User}
                               :users {:type '(non-null (list (non-null User)))}}
               :mutations     {:updateUser {:type :User
                                            :args {:input {:type :UserInput}}}}})

  (unreachable-input-types schema)

  )
