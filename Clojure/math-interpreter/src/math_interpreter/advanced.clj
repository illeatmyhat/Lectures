(ns math-interpreter.advanced)

(declare eval-math)

(defn apply-op [op args]
  (if (= 1 (count args))
    (eval-math (first args))
    (op (eval-math (first args)) (apply-op op (next args)))))

(defn apply-function [form]
  (let [op (first form)]
    (cond (= '+ op) (apply-op + (next form))
          (= '- op) (apply-op - (next form))
          (= '* op) (apply-op * (next form))
          (= '/ op) (apply-op / (next form))
          (= '% op) (apply-op mod (next form))
          :else form)))

(defn eval-math [form]
  (cond (number? form) form
        (list? form) (apply-function form)))
