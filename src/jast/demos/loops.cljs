(ns jast.demos.loops
  (:require [jast.util :as u]
            [jast.core :refer [jast->js-program
                               jast->js-expression]]
            [jast.tools :refer [unquotable]]))

(def jast-rand-fn
  (unquotable
   '(fn [min-or-max max]
      (if min-or-max
        (if max
          (+ min-or-max (* (- max min-or-max) (Math.random)))
          (* min-or-max (Math.random)))
        (Math.random)))))

(def jast-program
  (unquotable
   '((let arr [])
     (:for [i 10]
           (arr.push (window.rand)))
     (:while (< [@arr 0] 10)
             (++ [@arr 0]))
     (console.log arr)
     (:for-of [element arr]
              (console.log element)))))

(defn start-demo []
  (set! js/window.rand
        (js/eval (jast->js-expression jast-rand-fn)))
  (js/eval (u/log (jast->js-program jast-program))))

(defn init []
  (js/window.addEventListener "load" start-demo))
