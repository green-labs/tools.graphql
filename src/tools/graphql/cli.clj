(ns tools.graphql.cli
  (:require [clojure.tools.cli :refer [parse-opts]]
            [cli-matic.core :refer [run-cmd]]
            [tools.graphql.api :as api])
  (:gen-class))

(defn foo [& args]
  (prn args))

(def CONFIGURATION
  {:command     "tg"
   :subcommands [{:command     "edn2sdl"
                  :description "Convert an edn file to a SDL file."
                  :opts        [{:as      ":input-path"
                                 :option  "src"
                                 :default :present
                                 :type    :string}
                                {:option  "out"
                                 :as      ":output-path"
                                 :default "schema.graphql"
                                 :type    :string}]
                  :runs        (fn [{:keys [src out]}]
                                 (api/edn2sdl {:input-path  src
                                               :output-path out}))}
                 {:command     "stitch"
                  :description "Read all subschemas in the given directory and merge them into one (superschema)."
                  :opts        [{:option   "path"
                                 :default  :present
                                 :as       ":input-paths"
                                 :multiple true
                                 :type     :string}
                                {:option  "out"
                                 :default "superschema.edn"
                                 :as      ":output-path"
                                 :type    :string}
                                {:option  "pretty"
                                 :default false
                                 :as      ":pretty"
                                 :type    :flag}
                                {:option  "watch"
                                 :default false
                                 :as      ":watch"
                                 :type    :flag}
                                {:option  "source-map"
                                 :default false
                                 :as      ":source-map?"
                                 :type    :flag}]
                  :runs        (fn [{:keys [path out pretty watch source-map]}]
                                 (api/stitch-schemas {:input-paths path
                                                      :output-path out
                                                      :pretty      pretty
                                                      :watch       watch
                                                      :source-map? source-map}))}
                 {:command     "validate"
                  :description "Validate the edn GraphQL schema."
                  :opts        [{:option  "src"
                                 :default :present
                                 :as      ":input-path"
                                 :type    :string}]
                  :runs        (fn [{:keys [src]}]
                                 (api/validate {:input-path src}))}]})


(defn -main [& args]
  (run-cmd args CONFIGURATION)
  #_(prn (parse-opts args cli-options))
  #_(api/stitch-schemas {:input-paths ["test-resources/subschemas"]
                         :pretty      true
                         :watch       true}))

;(parse-opts ["stitch-schemas"] cli-options)