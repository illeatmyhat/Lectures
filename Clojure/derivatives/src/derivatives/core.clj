;; derivatives.core
;; To fix the problem where the insides of trig and log functions are in prefix notation
;; we need to make a big egregious list of every possible function and run make-function
;; on the insides
;; i.e. run make-power on the inside of (cot(** x 2))
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
  (cond
         (= a 0) b ; simplify 'a + 0'
         (= b 0) a ; simplify '0 + b'
         ; actually add if both are numbers
         (and (number? a) (number? b)) (+ a b)
  ; return the unsimplified product otherwise
  :else (list a '+ b)))

;; Selects the addend (first value) of a sum.
(defn addend [sum]
  (second sum))

;; Selects the augend (second value) of a sum.
(defn augend [sum]
  (third sum))

;; True if the form represents a diff.
(defn diff? [form]
  (and (list? form) (= '- (first form))))

;; Constructs a diff of a and b.
(defn make-diff [a b]
  (cond
        (= a 0) (list '- b) ; simplify '0 - b'
        (= b 0) a ; simplify 'a - 0'
        ; actually subtract if both are numbers
        (and (number? a) (number? b)) (- a b)
  ; return the unsimplified product otherwise
  :else (list a '- b)))

;; True if the form represents a product.
(defn prod? [form]
  (and (list? form) (= '* (first form))))

;; Constructs a product of a and b.
(defn make-prod [a b]
  (cond ; the product of any number and 0 is 0
        (= a 0) 0
        (= b 0) 0
        ; the product of any number and 1 is the number
        (= a 1) b
        (= b 1) a
        ; actually multiply if both are numbers
        (and (number? a) (number? b)) (* a b)
  ; return the unsimplified product otherwise
  :else (list a '* b)))

;; Selects the multiplier (first value) of a product.
(defn multiplier [prod]
  (second prod))

;; Selects the multiplicand (second value) of a product.
(defn multiplicand [prod]
  (third prod))

;; True if the form represents a power.
;; i.e. power is a constant
(defn power? [form]
  (and (list? form) (= '** (first form)) (number? (third form))))

;; True if the form represents the exponential function.
;; i.e. base is a constant
(defn exp? [form]
  (and (list? form) (= '** (first form)) (number? (second form))))
;; Constructs a power of a and b.
(defn make-power [a b]
  (cond
        (= a 0) 0
        (= a 1) 1
        (= b 0) 1
        (= b 1) a
        ; actually exponentiate if both are numbers
        (and (number? a) (number? b)) (Math/pow a b)
  ; return the unsimplified product otherwise
  :else (list a '** b)))

;; Selects the base (first value) of a power.
(defn base [power]
  (second power))

;; Selects the power (second value) of a power.
(defn power [power]
  (third power))

;; True if the form represents a quotient.
(defn quot? [form]
  (and (list? form) (= '/ (first form))))

;; Constructs a quotient of a and b.
(defn make-quot [a b]
  (cond
        (= a 0) 0 ; simplify for '0 / b'
        (= b 1) a ; simplify for 'a / 1'
        ; actually divide if both are numbers
        (and (number? a) (number? b)) (/ a b)
  ; return the unsimplified product otherwise
  :else (list a '/ b)))

;; Selects the dividend (first value) of a quotient.
(defn dividend [quot]
  (second quot))

;; Selects the divisor (second value) of a quotient.
(defn divisor [quot]
  (third quot))

;; True if the form represents a natural log.
(defn ln? [form]
  (and (list? form) (= 'ln (first form))))

;; Selects the u-value (first value) of a logarithm.
(defn log-of [ln]
  (second ln))

;; Constructs a secant with angle a.
(defn make-ln [a]
  (if (number? a) (Math/log a) (list 'ln a)))

;; True if the form represents a sine function.
(defn sin? [form]
  (and (list? form) (= 'sin (first form))))

;; Constructs a sine with angle a.
(defn make-sin [a]
  (list 'sin a))

;; True if the form represents a cosine function.
(defn cos? [form]
  (and (list? form) (= 'cos (first form))))

;; Constructs a cosine with angle a.
(defn make-cos [a]
  (list 'cos  a))

;; True if the form represents a tangent function.
(defn tan? [form]
  (and (list? form) (= 'tan (first form))))

;; Constructs a tangent with angle a.
(defn make-tan [a]
  (list 'tan  a))

;; True if the form represents a cosine function.
(defn sec? [form]
  (and (list? form) (= 'sec (first form))))

;; Constructs a secant with angle a.
(defn make-sec [a]
  (list 'sec  a))

;; True if the form represents a cosecant function.
(defn csc? [form]
  (and (list? form) (= 'csc (first form))))

;; Constructs a cosecant with angle a.
(defn make-csc [a]
  (list 'csc  a))

;; True if the form represents a cotangent function.
(defn cot? [form]
  (and (list? form) (= 'cot (first form))))

;; Constructs a cotangent with angle a.
(defn make-cot [a]
  (list 'cot  a))

;; Selects the u-value (inside) of a trig function.
(defn angle [trig]
  (second trig))

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
        ; Diff rule
        (diff? form) (make-diff (derivative (addend form) var)
                              (derivative (augend form) var))
        ; Product rule
        (prod? form) (make-sum (make-prod (multiplier form)
                                          (derivative (multiplicand form) var))
                               (make-prod (derivative (multiplier form) var)
                                          (multiplicand form)))
        ; Exponential Function
        (exp? form) (make-prod form
                               (make-prod (make-ln (base form))
                                           (derivative (power form) var)))
        ; Power rule
        (power? form) (make-prod (make-prod (power form)
                                            (make-power (base form) (- (power form) 1)))
                                 (derivative (base form) var))
        ; Quotient rule
        (quot? form) (make-quot (make-diff (make-prod (divisor form)
                                                     (derivative (dividend form) var))
                                          (make-prod (derivative (divisor form) var)
                                                     (dividend form)))
                                (make-power (divisor form) 2))
        ; Natural Logarithm
        (ln? form) (make-quot (derivative (log-of form) var)
                              (log-of form))
        ; Sine
        (sin? form) (make-prod (make-cos (angle form))
                               (derivative (angle form) var))
        ; Cosine
        (cos? form) (make-prod (make-diff 0 (make-sin (angle form)))
                               (derivative (angle form) var))
        ; Tangent
        (tan? form) (make-prod (make-power (make-sec (angle form)) 2)
                               (derivative (angle form) var))
        ; Secant
        (sec? form) (make-prod (derivative (angle form) var)
                               (make-prod (make-sec (angle form))
                                          (make-tan (angle form))))
        ; Cosecant
        (csc? form) (make-diff 0
                               (make-prod (derivative (angle form) var)
                                          (make-prod (make-csc (angle form))
                                                     (make-cot (angle form)))))
        ; Cotangent
        (cot? form) (make-diff 0
                               (make-prod (derivative (angle form) var)
                                          (make-power (make-cot (angle form))
                                                        2)))))