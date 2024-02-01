(ns jast.compiler
  (:require [clojure.string :refer [join]]
            [jast.tools :refer [clj-name->js]]))

(defn throw-str [s]
  (throw #?(:cljs s)
         #?(:clj (Exception. s))))

(def infix-ops
  '#{+ - / * % < > != == <= >= || && & "^^" "^" | << >>})

(def modifying-assigners
  '#{+= *= -= "/=" "^=" "%=" |= &= <<= >>=})

(defn parenthesize [s]
  (str "(" s ")"))

(defn expression->js [expression & [top-level]]
  (cond
    (symbol? expression) (clj-name->js expression)

    (or (number? expression) (boolean? expression)) (str expression)

    (string? expression) (str \" expression \")

    (vector? expression)
    (if (and (seq? (first expression))
             (= 'clojure.core/deref
                (first (first expression))))
      (let [[[_ array-expression] index-expression] expression]
        (str (expression->js array-expression)
             "["
             (expression->js index-expression)
             "]"))
      (str "["
           (join ", "
                 (map expression->js expression))
           "]"))

    (seq? expression)
    (let [[f & args] expression]
      (cond
        (= '++ f) (str (expression->js (first args)) "++")
        (= '-- f) (str (expression->js (first args)) "--")

        (and (= f '-) (= (count args) 1))
        (str "-" (expression->js (first args)))

        (and (= f '/) (= (count args) 1))
        (str "(1./" (expression->js (first args)) ")")

        (modifying-assigners f)
        (str (expression->js (first args))
             " "
             f
             " "
             (expression->js (second args)))

        (infix-ops f)
        (parenthesize (join (str " " f " ")
                            (map expression->js args)))

        (= 'if f)
        (let [[conditional true-expression false-expression] args]
          (parenthesize
           (str (expression->js conditional)
                " ? "
                (expression->js true-expression)
                " : "
                (expression->js false-expression))))

        (= '= f)
        (cond-> (str (expression->js (first args))
                     " = "
                     (expression->js (second args)))
          (not top-level) parenthesize)

        ('#{let const var} f)
        (str f
             " "
             (expression->js (first args))
             " = "
             (expression->js (second args)))

        (= 'fn f)
        (let [[arg-names & body] args]
          (parenthesize
           (str (if (= 1 (count arg-names))
                  (clj-name->js (first arg-names))
                  (str "("
                       (join "," (map clj-name->js arg-names))
                       ")"))
                " => "
                (if (= 1 (count body))
                  (expression->js (first body))
                  "JAST: multi-line functions don't work yet!"))))

        (= 'raw-js f)
        (str (first args))

        (and (symbol? f) (= ".-" (subs (str f) 0 2)))
        (str (expression->js (first args))
             "."
             (clj-name->js (subs (str f) 2)))

        (and (symbol? f) (= "." (first (str f))))
        (str (expression->js (first args))
             (clj-name->js f)
             (parenthesize
              (join ", "
                    (map expression->js (rest args)))))

        :else (str (expression->js f)
                   (parenthesize
                    (join ", "
                          (map expression->js args))))))

    :else (throw-str (str "jast: Can't parse expression: " expression))))

(defn is-statement-block? [statement]
  (and (seq? statement)
       (#{:if
          :when
          :else
          :else-if :elseif :elif
          :while
          :for
          :for-of
          :block}
        (first statement))))

(defn statement->lines [statement]
  (if (is-statement-block? statement)
    (let [[statement-type & statement-args] statement]
      (if (#{:if "if"} statement-type)
        (if (not= 3 (count statement-args))
          (throw-str
           (str "jast: Invalid number of argments to :if\n\nArguments given:"
                (apply str (interleave (repeat "\n") statement-args))))
          (let [[conditional & clauses] statement-args
                [true-lines false-lines]
                (map #(map (partial str "  ")
                           (if (and (seq? %)
                                    (= (first %) :block))
                             (mapcat statement->lines (rest %))
                             (statement->lines %)))
                     clauses)]
            (concat (list (str "if ("
                               (expression->js conditional)
                               ") {\n"))
                    true-lines
                    (list "}\n  else {\n")
                    false-lines
                    (list "}\n"))))
        (let [[block-start consumed-statement-args]
              (cond
                (= :when statement-type)
                [(str "if ("
                      (expression->js (first statement-args))
                      ") {")
                 1]

                (= :while statement-type)
                [(str "while ("
                      (expression->js (first statement-args))
                      ") {")
                 1]

                (= :else statement-type)
                ["else {" 0]

                (#{:else-if :elseif :elif}
                 statement-type)
                [(str "else if ("
                      (expression->js (first statement-args))
                      ") {")
                 1]

                (= :for statement-type)
                (if (vector? (first statement-args))
                  (let [loop-definition (first statement-args)
                        [binding-name & loop-args] loop-definition
                        [initial-value max-value increment-value]
                        (case (count loop-args)
                          1 [0 (first loop-args)]
                          2 loop-args
                          3 loop-args
                          (throw-str (str "jast: Invalid for loop definition "
                                          loop-definition)))
                        parse-value (fn [value]
                                      (if (number? value)
                                        (str value)
                                        (expression->js value)))]
                      [(str "for (let "
                            (expression->js binding-name)
                            " = "
                            (parse-value initial-value)
                            "; "
                            (expression->js binding-name)
                            " < "
                            (parse-value max-value)
                            "; "
                            (expression->js binding-name)
                            (if increment-value
                              (str " += " (parse-value increment-value))
                              "++")
                            ") {")
                       1])
                  [(str "for ("
                        (join "; "
                              (map expression->js
                                   (take 3 statement-args)))
                        ") {")
                   3])
                
                (= :for-of statement-type)
                [(let [[[var-name collection-name]] statement-args]
                   (str "for (let "
                        (expression->js var-name)
                        " of "
                        (expression->js collection-name)
                        ") {"))
                 1]

                (:block statement-type)
                ["{" 0])]
          (concat (list (str block-start "\n"))
                  (map (partial str "  ")
                       (mapcat statement->lines
                               (drop consumed-statement-args statement-args)))
                  (list "}\n")))))
    (if (and (seq? statement)
             (= (first statement) 'do))
      (mapcat statement->lines
              (rest statement))
      (try (list (str (expression->js statement true)
                      ";\n"))
           #?(:cljs (catch :default e
                      (throw
                       (ex-info
                        (str "jast: Error while compiling statement "
                             statement)
                        e))))
           #?(:clj (catch Exception e
                     (throw
                      (Exception.
                       (ex-info (str "jast: Error while compiling statement "
                                     statement)
                                e)))))))))


(defn compile-jast-expression [expression]
  (expression->js expression true))

(defn compile-jast-program [program]
  (join (mapcat statement->lines program)))
