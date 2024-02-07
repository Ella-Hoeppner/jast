(ns jast.demos.fns
  (:require [jast.util :as u]
            [jast.core :refer [jast->js-program
                               jast->js-expression]]
            [jast.tools :refer [unquotable]]))

(def jast-program
  (unquotable
   '((let f1 (fn [x y] (+ x 1)))
     (let f2 (fn [z]
               (= z (f1 z 5))
               z)))))

(defn start-demo []
  (js/eval (u/log (jast->js-program jast-program))))

(defn init []
  (js/window.addEventListener "load" start-demo))
