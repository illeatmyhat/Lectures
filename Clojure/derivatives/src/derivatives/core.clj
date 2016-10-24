(ns derivatives.core)

(defn third [list]
  (second (next list)))

;; True if the form is a variable (symbol).
(defn variable? [form]
  (symbol? form))

;; True if the two forms are the same variable.
(defn same-variable? [form1 form2]
  (and (variable? form1) (variable? form2) (= form1 form2)))

;; True if the form represents a sum.
(defn sum? [form]
  (and (list? form) (= '+ (first form))))

;; Constructs a sum of a and b.
(defn make-sum [a b]
  (list '+ a b))

;; Selects the addend (first value) of a sum.
(defn addend [sum]
  (second sum))

;; Selects the augend (second value) of a sum.
(defn augend [sum]
  (third sum))

;; True if the form represents a product.
(defn prod? [form]
  (and (list? form) (= '* (first form))))

;; Constructs a product of a and b.
(defn make-prod [a b]
  (list '* a b))

;; Selects the multiplier (first value) of a product.
(defn multiplier [prod]
  (second prod))

;; Selects the multiplicand (second value) of a product.
(defn multiplicand [prod]
  (third prod))

;; Returns the derivative of a function expressed in Clojure notation, where variables are quoted.
;; The second parameter is the variable which the derivative is calculated with respect to.
(defn derivative [form var]
  (cond ; The derivative of a constant is 0
        (number? form) 0
        ; The derivative of a variable is 0 if the variable is not the one being derived; or 1, if it is.
        (variable? form) (if (same-variable? form var) 1 0)
        ; Sum rule
        (sum? form) (make-sum (derivative (addend form) var)
                              (derivative (augend form) var))
        ; Product rule
        (prod? form) (make-sum (make-prod (multiplier form)
                                          (derivative (multiplicand form) var))
                               (make-prod (derivative (multiplier form) var)
                                          (multiplicand form)))))