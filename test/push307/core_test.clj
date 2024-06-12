(ns push307.core-test
  (:require [clojure.test :refer [deftest is]]
            [push411.core :as core]
            [push411.lib :as lib]
            [push411.regression :as reg]))

(deftest make-random-plushy-genome-test
  (is (every? true? (map (fn [inst] (some #(= % inst) lib/default-instructions))
                   (core/make-random-plushy-genome lib/default-instructions 20))))
  (is (every? true? (map (fn [inst] (some #(= % inst) lib/default-instructions))
                         (core/make-random-plushy-genome lib/default-instructions 20))))
  (is (every? true? (map (fn [inst] (some #(= % inst) lib/default-instructions))
                         (core/make-random-plushy-genome lib/default-instructions 20)))))

(def genome-a '(1 2 3 4 5))
(def genome-b '("a" "b" "c" "d" "e"))
(def genome-c '(:q :r :s :t :u :v :w :x :y :z))

(deftest crossover-test
  (is (every? true? (map (fn [gene] (or (some #(= % gene) genome-a)
                                        (some #(= % gene) genome-b)))
                         (core/crossover genome-a genome-b))))
  (is (every? true? (map (fn [gene] (or (some #(= % gene) genome-a)
                                        (some #(= % gene) genome-c)))
                         (core/crossover genome-a genome-c))))
  (is (every? true? (map (fn [gene] (or (some #(= % gene) genome-b)
                                        (some #(= % gene) genome-c)))
                         (core/crossover genome-b genome-c)))))

(deftest uniform-mutation-test
  (is (and (= (count (core/uniform-mutation genome-a lib/default-instructions))
              (count genome-a))
           (every? true? (map (fn [gene] (or (some #(= % gene) genome-a)
                                             (some #(= % gene) lib/default-instructions)))
                              (core/uniform-mutation genome-a lib/default-instructions)))))
  (is (and (= (count (core/uniform-mutation genome-b lib/default-instructions))
              (count genome-b))
           (every? true? (map (fn [gene] (or (some #(= % gene) genome-b)
                                             (some #(= % gene) lib/default-instructions)))
                              (core/uniform-mutation genome-b lib/default-instructions)))))
  (is (and (= (count (core/uniform-mutation genome-c lib/default-instructions))
              (count genome-c))
           (every? true? (map (fn [gene] (or (some #(= % gene) genome-c)
                                             (some #(= % gene) lib/default-instructions)))
                              (core/uniform-mutation genome-c lib/default-instructions))))))

(def example-population
  '({:genome ("highest error") :errors [100 100 100 100 100] :total-error 500}
    {:genome (72) :errors [9 18 0 9 9] :total-error 45}
    {:genome (4) :errors [0 5 20 15 10] :total-error 50}
    {:genome ("lowest error") :errors [1 0 0 0 1] :total-error 2}
    {:genome (:no-stack-item) :errors [5 5 5 5 5] :total-error 25}))

(deftest get-best-individual-test
  (is (=
       (core/get-best-individual example-population)
       {:genome '("lowest error") :errors [1 0 0 0 1] :total-error 2})))

(def solution-genome '(push411.lib/in1 push411.lib/in1 push411.lib/in1 push411.lib/integer_* push411.lib/integer_* push411.lib/in1 3 push411.lib/integer_+ push411.lib/integer_+))
(def off-by-1 '(push411.lib/in1 push411.lib/in1 push411.lib/in1 push411.lib/integer_* push411.lib/integer_* push411.lib/in1 4 push411.lib/integer_+ push411.lib/integer_+))

(deftest regression-error-function-test
  (is (=
       (core/regression-error-function
        {:genome solution-genome})
       {:genome solution-genome :program solution-genome
        :errors (vec (take (count reg/cases) (repeat 0)))
        :total-error 0}))
  (is (=
       (core/regression-error-function
        {:genome off-by-1})
       {:genome off-by-1 :program off-by-1
        :errors (vec (take (count reg/cases) (repeat 1)))
        :total-error (count reg/cases)})))
