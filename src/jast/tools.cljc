(ns jast.tools
  #?(:clj (:require [clojure.walk :refer [prewalk-replace
                                          prewalk]]))
  #?(:cljs (:require-macros [jast.tools])))

(defn multichar-escape [s escape-pairs]
  (loop [escaped-str ""
         remaining-str s]
    (if (empty? remaining-str)
      escaped-str
      (if-let [[new-escaped-str new-remaining-str]
               (some (fn [[replace-str replacement-str]]
                       (when (and (>= (count remaining-str)
                                      (count replace-str))
                                  (= (subs remaining-str 0 (count replace-str))
                                     replace-str))
                         [(str escaped-str replacement-str)
                          (subs remaining-str (count replace-str))]))
                     escape-pairs)]
        (recur new-escaped-str new-remaining-str)
        (recur (str escaped-str (first remaining-str))
               (subs remaining-str 1))))))

(defn clj-name->js [clj-name]
  (let [name-str (str clj-name)
        leading-dash? (= (first name-str) "-")]
    (cond->> (multichar-escape (cond-> name-str
                                 (or leading-dash? (keyword? clj-name))
                                 (subs 1))
                               [["->" "ARROW"]
                                ["-" "_"]
                                ["?" "QUESTION_MARK"]])
      leading-dash? (str "-"))))

#?(:clj
   (defmacro unquotable [& expressions]
     (let [quote-replacement (gensym 'jast_REPLACED_QUOTE)
           splice-keyword (keyword (gensym 'splice))]
       (letfn [(inline-unquotes
                 [form]
                 (let [replacement-map-atom (atom {})
                       inlined-replacements-form
                       (doall
                        (prewalk
                         (fn [subform]
                           (if (and (seq? subform)
                                    ('#{clojure.core/unquote
                                        clojure.core/unquote-splicing}
                                     (first subform)))
                             (let [replacement-binding (keyword (gensym))]
                               (swap! replacement-map-atom
                                      assoc
                                      replacement-binding
                                      (second subform))
                               (if (= (first subform)
                                      'clojure.core/unquote)
                                 replacement-binding
                                 (list splice-keyword replacement-binding)))
                             subform))
                         form))]
                   (list 'clojure.walk/prewalk
                         (prewalk-replace
                          {:splice-symbol splice-keyword}
                          '(fn [form]
                             (if (sequential? form)
                               (cond->
                                (reduce (fn [new-form subform]
                                          (if (and (list? subform)
                                                   (= (first subform)
                                                      :splice-symbol))
                                            (into new-form (second subform))
                                            (conj new-form subform)))
                                        []
                                        form)
                                 (not (vector? form)) seq)
                               form)))
                         (list 'clojure.walk/prewalk-replace
                               @replacement-map-atom
                               (list `quote
                                     (replace-quotes
                                      inlined-replacements-form))))))
               (replace-quotes
                 [form]
                 (if (and (seq? form)
                          (= (first form)
                             quote-replacement))
                   (let [subform (second form)]
                     (if (coll? subform)
                       (inline-unquotes subform)
                       (list `quote subform)))
                   form))]
         (->> expressions
              (prewalk-replace {`quote quote-replacement})
              (prewalk replace-quotes)
              (cons 'do))))))
