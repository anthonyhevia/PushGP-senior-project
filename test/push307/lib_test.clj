(ns push307.lib-test
  (:require [clojure.test :refer [deftest is]]
            [push411.util :as u]
            [push411.lib :as lib]))


(def example-push-state
  {:exec '(push411.lib/integer_+ push411.lib/integer_-)
   :integer '(1 2 3 4 5 6 7)
   :string '("abc" "def")
   :input {:in1 4 :in2 6}})

(def example-push-state2
  {:exec '(push411.lib/exec_dup push411.lib/integer_+ push411.lib/integer_-)
   :integer '(80)
   :string '("alphabet" "string2")
   :input {:in1 1 :in2 18}})

(def example-push-state3
  {:exec '(push411.lib/exec_dup push411.lib/integer_+ push411.lib/integer_-)
   :integer '(24 80)
   :string '("alphabet" "string2")
   :input {:in1 1 :in2 18}})

(deftest in1-test
  (is (=
       (lib/in1 example-push-state)
       {:exec '(4 push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (lib/in1 u/empty-push-state)
       {:exec '(nil)
        :integer '()
        :string '()
        :boolean '()
        :vec-integer '()
        :input {}})))

(deftest integer_--test
  (is (=
       (lib/integer_- example-push-state)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (lib/integer_- example-push-state2)
       example-push-state2))
  (is (=
       (lib/integer_- u/empty-push-state)
       u/empty-push-state))
  (is (=
       (lib/integer_- {:exec '(push411.lib/integer_+ push411.lib/integer_-)
                       :integer '(9223372036854775807 -1)
                       :string '("abc" "def")
                       :input {:in1 4 :in2 6}})
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(-9223372036854775808)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (lib/integer_- {:exec '(push411.lib/integer_+ push411.lib/integer_-)
                       :integer '(-2 9223372036854775807)
                       :string '("abc" "def")
                       :input {:in1 4 :in2 6}})
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(9223372036854775809N)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}})))

(deftest integer_*-test
  (is (=
       (lib/integer_* example-push-state)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (lib/integer_* example-push-state2)
       example-push-state2))
  (is (=
       (lib/integer_* u/empty-push-state)
       u/empty-push-state))
  (is (=
       (lib/integer_* {:exec '(push411.lib/integer_+ push411.lib/integer_-)
                       :integer '(2 4700000000000000000)
                       :string '("abc" "def")
                       :input {:in1 4 :in2 6}})
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(9400000000000000000N)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}})))

(deftest integer_%-test
  (is (=
       (lib/integer_% example-push-state)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (lib/integer_% example-push-state2)
       example-push-state2))
  (is (=
       (lib/integer_% example-push-state3)
       {:exec '(push411.lib/exec_dup push411.lib/integer_+ push411.lib/integer_-)
        :integer '(3)
        :string '("alphabet" "string2")
        :input {:in1 1 :in2 18}})))

(def bool-test-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '(true false true)
   :input {}})

(deftest boolean-instructions-test
  (is (=
       (push411.lib/boolean_and bool-test-state)
       {:exec '()
        :integer '()
        :string '()
        :boolean '(false true)
        :input {}}))
  (is (=
       (push411.lib/boolean_or bool-test-state)
       {:exec '()
        :integer '()
        :string '()
        :boolean '(true true)
        :input {}}))
  (is (=
       (push411.lib/boolean_not bool-test-state)
       {:exec '()
        :integer '()
        :string '()
        :boolean '(false false true)
        :input {}}))
  (is (=
       (push411.lib/boolean_=
        {:exec '()
         :integer '()
         :string '()
         :boolean '(false false true)
         :input {}})
       {:exec '()
        :integer '()
        :string '()
        :boolean '(true true)
        :input {}})))

(def comparison-test-state
  {:exec '()
   :integer '(8 6 7 5 3 0 9)
   :string '()
   :boolean '()
   :input {}})

(def comparison-test-state2
  {:exec '()
   :integer '(3 3 3 5 3 0 9)
   :string '()
   :boolean '()
   :input {}})

(deftest comparison_instructions_test
  (is (=
       (lib/integer_< comparison-test-state)
       {:exec '()
        :integer '(7 5 3 0 9)
        :string '()
        :boolean '(true)
        :input {}}))
  (is (=
       (lib/integer_< comparison-test-state2)
       {:exec '()
        :integer '(3 5 3 0 9)
        :string '()
        :boolean '(false)
        :input {}}))
  (is (=
       (lib/integer_> comparison-test-state)
       {:exec '()
        :integer '(7 5 3 0 9)
        :string '()
        :boolean '(false)
        :input {}}))
  (is (=
       (lib/integer_> comparison-test-state2)
       {:exec '()
        :integer '(3 5 3 0 9)
        :string '()
        :boolean '(false)
        :input {}}))
  (is (=
       (lib/integer_= comparison-test-state)
       {:exec '()
        :integer '(7 5 3 0 9)
        :string '()
        :boolean '(false)
        :input {}}))
  (is (=
       (lib/integer_= comparison-test-state2)
       {:exec '()
        :integer '(3 5 3 0 9)
        :string '()
        :boolean '(true)
        :input {}}))
  )

(def vec-test-state
  {:exec '()
   :integer '(3)
   :boolean '()
   :vec-integer '([10 20 30 40 50])})

(def vec-test-state2
  {:exec '()
   :integer '()
   :boolean '()
   :vec-integer '([1])})

(def vec-test-state3
  {:exec '()
   :integer '()
   :boolean '()
   :vec-integer '([])})

;; Before testing vector instructions, need to decide which will
;; pop vectors (if any) and which won't.
(deftest vec-instructions-test
;; vec_count tests
  (is (= (lib/vec_count vec-test-state)
         {:exec '()
          :integer '(5 3)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_count vec-test-state2)
         {:exec '()
          :integer '(1)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_count vec-test-state3)
         {:exec '()
          :integer '(0)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_count u/empty-push-state)
         u/empty-push-state))
  ;; vec_first tests
  (is (= (lib/vec_first vec-test-state)
         {:exec '()
          :integer '(10 3)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_first vec-test-state3)
         vec-test-state3))
  ;; vec_last tests
  (is (= (lib/vec_last vec-test-state)
         {:exec '()
          :integer '(50 3)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_last vec-test-state3)
         vec-test-state3))
  ;; vec_rest tests
  (is (= (lib/vec_rest vec-test-state)
         {:exec '()
          :integer '(3)
          :boolean '()
          :vec-integer '([20 30 40 50])}))
  (is (= (lib/vec_rest vec-test-state2)
         {:exec '()
          :integer '()
          :boolean '()
          :vec-integer '([])}))
  (is (= (lib/vec_rest vec-test-state3)
         {:exec '()
          :integer '()
          :boolean '()
          :vec-integer '([])}))
  ;; vec_nth tests
  (is (= (lib/vec_nth vec-test-state)
         {:exec '()
          :integer '(40)
          :boolean '()
          :vec-integer '()}))
  ;; test index too large
  (is (= (lib/vec_nth (u/push-to-stack vec-test-state :integer 10))
         {:exec '()
          :integer '(50 3)
          :boolean '()
          :vec-integer '()}))
  ;; test negative index
  (is (= (lib/vec_nth (u/push-to-stack vec-test-state :integer -25))
         {:exec '()
          :integer '(10 3)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_nth u/empty-push-state)
         u/empty-push-state))
  ;; vec_dup tests
  (is (= (lib/vec_dup vec-test-state)
         {:exec '()
          :integer '(3)
          :boolean '()
          :vec-integer '([10 20 30 40 50] [10 20 30 40 50])}))
  (is (= (lib/vec_dup vec-test-state3)
         {:exec '()
          :integer '()
          :boolean '()
          :vec-integer '([] [])}))
  ;; vec_iterate tests
  )

(def exec-test-state
  {:exec '(push411.lib/integer_+ push411.lib/integer_*)
   :integer '(3 5)
   :boolean '(true)
   :vec-integer '()})

(def exec-test-state2
  {:exec '(push411.lib/integer_+ push411.lib/integer_*)
   :integer '(3 5)
   :boolean '(false)
   :vec-integer '()})

(def exec-test-state3
  {:exec '(push411.lib/integer_+ push411.lib/integer_*)
   :integer '(3 5)
   :boolean '()
   :vec-integer '()})

(deftest exec-instructions-test
  ;; exec_dup tests
  (is (= (lib/exec_dup exec-test-state)
         {:exec '(push411.lib/integer_+ push411.lib/integer_+ push411.lib/integer_*)
          :integer '(3 5)
          :boolean '(true)
          :vec-integer '()}))
  (is (= (lib/exec_dup u/empty-push-state)
         u/empty-push-state))
  ;; exec_if tests
  (is (= (lib/exec_if exec-test-state)
         {:exec '(push411.lib/integer_+)
          :integer '(3 5)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/exec_if exec-test-state2)
         {:exec '(push411.lib/integer_*)
          :integer '(3 5)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/exec_if exec-test-state3)
         exec-test-state3)))

(def iterate-test-state
  {:exec '(push411.lib/integer_+)
   :integer '(0)
   :boolean '()
   :vec-integer '([10 20 30 40 50])})

(def iterate-test-state2
  {:exec '(push411.lib/integer_+)
   :integer '(0)
   :boolean '()
   :vec-integer '([10])})

(def iterate-test-state3
  {:exec '(push411.lib/integer_+)
   :integer '(0)
   :boolean '()
   :vec-integer '([])})

(def iterate-test-state4
  {:exec '()
   :integer '(0)
   :boolean '()
   :vec-integer '([10 20 30 40 50])})

(deftest vec_iterate-test
  (is (= (lib/vec_iterate iterate-test-state)
         {:exec '(push411.lib/integer_+ push411.lib/vec_iterate push411.lib/integer_+)
          :integer '(10 0)
          :boolean '()
          :vec-integer '([20 30 40 50])}))
  (is (= (lib/vec_iterate iterate-test-state2)
         {:exec '(push411.lib/integer_+ push411.lib/vec_iterate push411.lib/integer_+)
          :integer '(10 0)
          :boolean '()
          :vec-integer '([])}))
  (is (= (lib/vec_iterate iterate-test-state3)
         {:exec '()
          :integer '(0)
          :boolean '()
          :vec-integer '()}))
  (is (= (lib/vec_iterate iterate-test-state4)
         {:exec '()
          :integer '(0)
          :boolean '()
          :vec-integer '([10 20 30 40 50])}))
  )