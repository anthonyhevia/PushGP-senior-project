(ns push307.util-test
  (:require [clojure.test :refer [deftest is]]
            [push411.util :as u]))

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

(deftest push-to-stack-test
  (is (=
       (u/push-to-stack example-push-state :exec '(5 1 push411.lib/integer_+))
       {:exec '((5 1 push411.lib/integer_+) push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/push-to-stack example-push-state :integer 10)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(10 1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/push-to-stack example-push-state :vec-integer [10 20 30 40])
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}
        :vec-integer '([10 20 30 40])}))
  (is (=
       (u/push-to-stack example-push-state :string "new string")
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("new string" "abc" "def")
        :input {:in1 4 :in2 6}})))

(deftest pop-stack-test
  (is (=
       (u/pop-stack example-push-state :exec)
       {:exec '(push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/pop-stack example-push-state :integer)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/pop-stack example-push-state :string)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/pop-stack
        {:exec '((5 1 push411.lib/integer_+) push411.lib/integer_+ push411.lib/integer_-)
         :integer '(1 2 3 4 5 6 7)
         :string '("abc" "def")
         :input {:in1 4 :in2 6}}
        :exec)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}})))

(deftest nth-pop-stack-test
  (is (=
       (u/nth-pop-stack example-push-state :integer 4)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/nth-pop-stack example-push-state :integer 1)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/nth-pop-stack example-push-state :integer 6)
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}})))

(deftest peek-stack?-test
  (is (=
       'push411.lib/integer_+
       (u/peek-stack example-push-state :exec)))
  (is (=
       :no-stack-item
       (u/peek-stack u/empty-push-state :exec)))
  (is (=
       1
       (u/peek-stack example-push-state :integer)))
  (is (=
       "abc"
       (u/peek-stack example-push-state :string))))

(deftest push-code-block-test
  (is (=
       (u/push-code-block example-push-state '(9 10 11 "ghi" push411.lib/exec_dup))
       {:exec '(9 10 11 "ghi" push411.lib/exec_dup push411.lib/integer_+ push411.lib/integer_-)
        :integer '(1 2 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/push-code-block u/empty-push-state (get example-push-state :exec))
       {:exec '(push411.lib/integer_+ push411.lib/integer_-)
        :integer '()
        :string '()
        :boolean '()
        :vec-integer '()
        :input {}})))

(deftest interpret-one-step-test
  (is (=
       (u/interpret-one-step example-push-state)
       {:exec '(push411.lib/integer_-)
        :integer '(3 3 4 5 6 7)
        :string '("abc" "def")
        :input {:in1 4 :in2 6}}))
  (is (=
       (u/interpret-one-step example-push-state2)
       {:exec '(push411.lib/integer_+ push411.lib/integer_+ push411.lib/integer_-)
        :integer '(80)
        :string '("alphabet" "string2")
        :input {:in1 1, :in2 18}}))
  (is (=
       (u/interpret-one-step {:exec '(777 push411.lib/integer_-)
                                :integer '(80)
                                :string '("alphabet" "string2")
                                :input {:in1 1, :in2 18}})
       {:exec '(push411.lib/integer_-)
        :integer '(777 80)
        :string '("alphabet" "string2")
        :input {:in1 1, :in2 18}}))
  (is (=
       (u/interpret-one-step {:exec '(("hello" 1 2 3 push411.lib/exec_dup) push411.lib/integer_-)
                                :integer '(80)
                                :string '("alphabet" "string2")
                                :input {:in1 1, :in2 18}})
       {:exec '("hello" 1 2 3 push411.lib/exec_dup push411.lib/integer_-)
        :integer '(80)
        :string '("alphabet" "string2")
        :input {:in1 1, :in2 18}})))

(def example-push-program
  '(3 5 push411.lib/integer_* false push411.lib/exec_dup ("hello" 4 true "world" push411.lib/integer_-)))

(deftest interpret-push-program-test
  (is (=
       (u/interpret-push-program example-push-program u/empty-push-state)
       {:exec '()
        :integer '(7)
        :string '("world" "hello" "world" "hello")
        :boolean '(true true false)
        :vec-integer '()
        :input {}}))
  (is (=
       (u/interpret-push-program
        '(3 5 push411.lib/integer_* push411.lib/exec_dup ("hello" push411.lib/in1 "world" push411.lib/integer_-))
        {:exec '() :integer '() :string '() :input {:in1 5}})
       {:exec '()
        :integer '(5)
        :string '("world" "hello" "world" "hello")
        :input {:in1 5}})))