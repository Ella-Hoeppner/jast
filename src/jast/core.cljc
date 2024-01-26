(ns jast.core
  (:require [jast.compiler :refer [compile-jast]]
            [jast.macros :refer [default-macros]]))

(defn apply-macros [program macros]
  program)

(defn jast->js
  ([program]
   (-> program
       (apply-macros default-macros)
       compile-jast))
  ([program macros & [exclude-defaults?]]
   (-> program
       (apply-macros (cond->> macros
                       (not exclude-defaults?)
                       (merge default-macros)))
       compile-jast)))
