(ns tools.graphql.inspect
  (:require [tools.graphql.sdl :as sdl]
            [clojure.string :as s]))

(defn inner-type
  [expr]
  (if (seq? expr)
    (let [[s1 s2] expr]
      (if s2
        (inner-type s2)
        s1))
    expr))

(defn get-object
  [schema type-name]
  (or (get-in schema [:objects (keyword type-name)])
      (get-in schema [:objects (symbol type-name)])))

(defn args->query-args
  ([args]
   (args->query-args args nil))
  ([args prefix]
   (map (fn [[k v]]
          [(str "$" prefix (name k))
           (select-keys v [:type :default-value])])
        args)))

(defn field-def->type
  [[_ {:keys [type]}]]
  (inner-type type))

(defn field-def->name
  [[field-name _]]
  (when field-name (name field-name)))

(defn field-def->args-str
  [[field-name {:keys [args]}]]
  (when args (sdl/->args args field-name)))

(defn type->field-def
  [schema type]
  [nil (-> (get-object schema type)
           (assoc :type type))])

(defn implementation-types
  "인터페이스 구현체를 모두 찾는다."
  [schema interface]
  (let [objects (-> schema :objects)]
    (->> (keys objects)
         (keep (fn [object-name]
                 (let [object (get objects object-name)]
                   (when (some #(= % interface) (:implements object))
                     object-name)))))))

(defmulti select-field
  "쿼리에서 선택하는 필드의 종류에 따른 필드 쿼리 문자열 생성
   Arguments
    - schema: lacinia 스키마 edn
    - field-def: 필드 정의 (objects에서 조회된 값. )
      - [field-name {:keys [type args fields]}]]
    - opts: option map
      - max-depth: 쿼리문을 생성할 최대 깊이
      - fields-map: 조회할 필드들을 map으로 조회합니다. (object type 만 지원)"
  (fn [schema field-def opts]
    (let [type (field-def->type field-def)]
      (cond
        (> (:depth opts) (:max-depth opts)) :max-depth
        (get-in schema [:unions type]) :union
        (get-in schema [:interfaces type]) :interface
        (or (get-in schema [:objects (keyword type)])
            (get-in schema [:objects (name type)])) :object))))

(defmethod select-field :union
  [schema field-def opts]
  (let [members  (get-in schema [:unions (field-def->type field-def) :members])
        children (->> members
                      (map #(let [children  (select-field schema (type->field-def schema %) opts)]
                              (when (not-empty children)
                                (str "... on " (name %)
                                     children))))
                      (filter #(not-empty %)))]
    (when (not-empty children)
      (str (field-def->name field-def)
           (when (not-empty children)
             (str " {\n" (s/join "\n" children) "\n}"))))))

(defmethod select-field :interface
  [schema field-def opts]
  (let [implementation-types (implementation-types schema (field-def->type field-def))
        children             (->> implementation-types
                                  (map #(let [children (select-field schema (type->field-def schema %) opts)]
                                          (when (not-empty children)
                                            (str "... on " (name %)
                                                 children))))
                                  (filter #(not-empty %)))]
    (when (not-empty children)
      (str (field-def->name field-def)
           (when (not-empty children)
             (str " {\n" (s/join "\n" children) "\n}"))))))

(defmethod select-field :object
  [schema field-def opts]
  (let [fields-map (:fields-map opts)
        object     (get-object schema (field-def->type field-def))
        fields     (get object :fields {})
        children   (->> (if-let [select-fields (keys fields-map)]
                          (select-keys fields select-fields)
                          fields)
                        (map (fn [field]
                               (str (select-field schema field (-> opts
                                                                   (update :depth inc)
                                                                   (update :fields-map (first field)))))))
                        (filter #(not-empty %)))]
    (when (not-empty children)
      (let [children' (if (:typename? opts) (conj children "__typename") children)]
        (str (when-let [field-name (field-def->name field-def)]
               (str field-name
                    (field-def->args-str field-def)))
             (str " {\n" (s/join "\n" children') "\n}"))))))

(defmethod select-field :default
  [_schema field-def _opts]
  (when-let [field-name (field-def->name field-def)]
    (str field-name
         (field-def->args-str field-def))))

(defmethod select-field :max-depth
  [_schema _field-def _opts])

(defn extract-field-args
  "재귀적으로 `max-depth`까지 필드의 인자를 모두 찾아서 반환"
  ([schema type]
   (extract-field-args schema type {:max-depth 3}))
  ([schema type {:keys [depth max-depth fields-map]
                 :or   {depth 0}
                 :as   opts}]
   (let [return-only-type (inner-type type)]
     (cond
       (< max-depth depth) []

       (get-in schema [:unions type])
       (->> (get-in schema [:unions return-only-type :members])
            (mapcat (fn [type] (extract-field-args schema type opts))))

       (get-in schema [:interfaces type])
       (->> (implementation-types schema return-only-type)
            (mapcat (fn [type] (extract-field-args schema type opts))))

       :else
       (let [fields (get (get-object schema return-only-type) :fields {})]
         (->> (if-let [select-fields (keys fields-map)]
                (select-keys fields select-fields)
                fields)
              (mapcat (fn [[field-name {:keys [type args]}]]
                        (concat
                         (when (get-object schema (inner-type type))
                           (extract-field-args schema type (assoc opts
                                                                  :depth (inc depth)
                                                                  :fields-map (field-name fields-map))))
                         (when (and args (<= depth max-depth))
                           (args->query-args args (name field-name))))))))))))

(defn- query&mutation->query
  [schema query-type query-name {:keys [args type]} {:keys [max-depth fields-map]
                                                     :or   {max-depth 3}
                                                     :as   opts}]
  (let [fields-map       (get fields-map query-name)
        return-only-type (inner-type type)
        query-args       (args->query-args args)
        field-args       (extract-field-args schema return-only-type {:max-depth  max-depth
                                                                      :fields-map fields-map})]
    (str query-type " " (name query-name)
         (sdl/->arg (->> (concat query-args field-args)
                         (into {})))
         " {\n"
         (name query-name)
         (when (seq args)
           (sdl/->args args))
         (select-field schema (type->field-def schema return-only-type) (merge opts
                                                                               {:depth      0
                                                                                :max-depth  max-depth
                                                                                :fields-map fields-map}))
         "\n}\n")))

(defn ->query
  "lacinia edn 에 있는 query 중 query-name 을 찾아 GraphQL에 요청 할 수 있는 query를 만듭니다.
  input
  - schema: lacinia edn
  - query-name: edn 에 지정된 query 이름
  - opts:
    - max-depth: 쿼리문을 생성할 최대 깊이
    - typename?: __typename 필드를 추가할지 여부
    - fields-map: 조회할 field를 지정합니다. 최상위를 포함해야합니다. object만 지원합니다. ex: {:user {:id nil}}, nil은 하위 전체를 의미합니다.
  output
  - GraphQL query string"
  ([schema query-name]
   (->query schema query-name {}))
  ([schema query-name opts]
   (when-let [query-def (or (get-in schema [:queries query-name])
                            (get-in schema [:objects :Query :fields query-name]))]
     (query&mutation->query schema "query" query-name query-def opts))))

(defn ->query-interface
  "인터페이스의 특정 구현체를 조회하는 쿼리. 해당 쿼리의 반환값은 인터페이스여야 한다.
   node쿼리와 같이 많은 타입을 리턴하는 경우 불필요한 모든 타입을 쿼리하는 경우를 방지한다.

   Arguments:
   - query-name: 쿼리명. eg. :node
   - imple-type: 조회할 해당 인터페이스의 구현체 목록. eg. [:CommunityPost :DirectDealPost]"
  [schema
   {:keys [query-name
           impl-types]}
   & {:keys [max-depth]
      :or   {max-depth 3}
      :as   opts}]
  (when-let [{:keys [args type]} (or (get-in schema [:queries query-name])
                                     (get-in schema [:objects :Query :fields query-name]))]
    (let [query-args (args->query-args args)
          field-def  (type->field-def schema type)
          field-args (mapcat #(extract-field-args schema % {:max-depth max-depth
                                                            :depth     2}) ;; 현재 깊이에 interface와 첫 필드 레벨 반영.
                             impl-types)
          opts       (merge {:depth     0
                             :max-depth max-depth} opts)]
      (str "query " (name query-name)
           (sdl/->arg (->> (concat query-args field-args)
                           (into {})))
           " {\n"
           (name query-name)
           (when (seq args)
             (sdl/->args args))
           (let [children (->> impl-types
                               (map #(let [children (select-field schema (type->field-def schema %) opts)]
                                       (when (not-empty children)
                                         (str "... on " (name %)
                                              children))))
                               (filter #(not-empty %)))]
             (when (not-empty children)
               (str (field-def->name field-def)
                    (str " {\n" (s/join "\n" children) "\n}"))))
           "\n}\n"))))

(defn ->mutation
  "lacinia edn 에 있는 mutation 중 mutation-name 을 찾아 GraphQL에 요청 할 수 있는 mutation을 만듭니다.
  input
  - schema: lacinia edn
  - mutation-name: edn 에 지정된 mutation 이름
   - opts:
    - max-depth: 쿼리문을 생성할 최대 깊이
  output
  - GraphQL mutation string"
  ([schema mutation-name]
   (->mutation schema mutation-name {}))
  ([schema mutation-name opts]
   (when-let [mutation-def (or (get-in schema [:mutations mutation-name])
                               (get-in schema [:objects :Mutation :fields mutation-name]))]
     (query&mutation->query schema "mutation" mutation-name mutation-def opts))))
