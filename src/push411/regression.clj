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

(def test-set-inputs
  (vec (range -1000 1001)))

(def test-set-expected-outputs
  (map target-function test-set-inputs))

(def test-set-cases (map vector test-set-inputs test-set-expected-outputs))

(def cases (map vector inputs expected-outputs))