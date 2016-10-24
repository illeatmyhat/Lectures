(ns math-interpreter.intermediate)

; a "forward declaration", like in C++
(declare eval-math)

(defn eval-sum [arguments]
  (if (= (count arguments) 1)
    (eval-math (first arguments))
    (+ (eval-math (first arguments)) (eval-sum (next arguments)))))

(defn eval-prod [arguments]
  (if (= (count arguments) 1)
    (eval-math (first arguments))
    (* (eval-math (first arguments)) (eval-prod (next arguments)))))

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
        (eval-sum (next form))

        (= '* op)
        (eval-prod (next form))

        ;; PROBLEMS:
        ;; DON'T REPEAT YOURSELF!! eval-sum vs eval-prod.
        )
      )
    )
  )