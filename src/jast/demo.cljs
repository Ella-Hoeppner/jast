(ns jast.demo
  (:require [jast.core :as j]
            [jast.tools :refer [unquotable]]))

(def jast-example-program
  (unquotable
   '((= x 1)
     (++ x))))

(defn test-jast [program]
  (js/console.log (j/jast->js program)))

(defn init []
  (js/window.addEventListener
   "load"
   (fn []
     (test-jast jast-example-program))))
