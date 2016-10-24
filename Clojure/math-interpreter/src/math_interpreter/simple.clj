(ns math-interpreter.simple)

; a "forward declaration", like in C++
(declare eval-math)

(defn third [coll]
  (second (next coll)))

; given a quoted Clojure form involving arithmetic operators and values, evaluate the form's value.
(defn eval-math [form]
  (cond
    ; If the form is just a number, then the value of the form is the number itself.
    (number? form)
    form

    ; If the form is a list, then it is a math function that must be applied to its arguments.
    (list? form)
    (let [op (first form)]
      ; op is the operator symbol
      (cond
        ; Determine which math operator, then apply its rules to find its value.
        (= '+ op)
        (+ (second form) (third form))

        (= '* op)
        (* (second form) (third form))

        ;; PROBLEMS:
        ;; Does not handle variable number of arguments in arithmetic forms.
        ;; Does not handle all operators (obvious fix).
        ;; Does not handle nested function calls!!!
        )
      )
    )
  )