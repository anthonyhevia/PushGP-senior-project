(ns push411.regression)

(comment
  "NAME: Anthony Hevia

   Regression
   Stores the symbolic regression training data
   ")


(defn target-function
  "Target function: f(x) = x^3 + x + 3
  Should literally compute this mathematical function."
  [x]
  (+ 3 x (* x x x)))

(def inputs
  (vec (range -20 21)))

(def expected-outputs
  (map target-function inputs))

(def cases (map hash-map inputs expected-outputs))