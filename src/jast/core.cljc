(ns jast.core
  (:require [jast.compiler :refer [compile-jast-program
                                   compile-jast-expression]]
            [jast.macros :refer [default-macros]]))

(defn apply-macros [program macros]
  program)

(defn jast->js-expression
  ([jast-expression]
   (-> jast-expression
       (apply-macros default-macros)
       compile-jast-expression))
  ([program macros & [exclude-defaults?]]
   (-> program
       (apply-macros (cond->> macros
                       (not exclude-defaults?)
                       (merge default-macros)))
       compile-jast-expression)))

(defn jast->js-program
  ([jast-program]
   (-> jast-program
       (apply-macros default-macros)
       compile-jast-program))
  ([jast-program macros & [exclude-defaults?]]
   (-> jast-program
       (apply-macros (cond->> macros
                       (not exclude-defaults?)
                       (merge default-macros)))
       compile-jast-program)))
