(ns jast.demo
  (:require [jast.core :as j]
            [jast.tools :refer [unquotable]]))

(defn test-jast [program]
  (js/console.log (j/jast->js program)))

(defn init []
  (js/window.addEventListener
   "load"
   (fn []
     (test-jast '((= x 1)
                  (++ x))))))
