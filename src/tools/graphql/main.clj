(ns tools.graphql.main
  (:require [tools.graphql.api :as api])
  (:gen-class))

(defn -main [& args]
  (api/stitch-schemas {:input-paths ["test-resources/subschemas"]
                       :pretty      true
                       :watch       true}))
