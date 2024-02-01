(ns jast.util)

(defn log [& vals]
  (doseq [val vals]
    (js/console.log (str val)))
  (last vals))
