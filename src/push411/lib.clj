(ns push411.lib
  (:require [push411.util :as u]))

(comment
  "NAME: Anthony Hevia

   Instruction Library
   Implements Push instructions, and defines a list
   of default instructions. Stores how many code blocks
   a given Push instruction should open (if any).
   ")

;;;;;;;;;;
; Instructions must all be either functions that take one Push state and return another
; or constant literals.
; The exception is `close`, which is only used in translation from Plushy to Push

(def default-instructions
  (list
   `integer_+
   `integer_-
   `integer_*
   `integer_%

   `integer_=
   `integer_<
   `integer_>

   `boolean_and
   `boolean_or
   `boolean_not
   `boolean_=

   `exec_dup
   `exec_if

   `vec_dup
   `vec_count
   `vec_empty?
   `vec_first
   `vec_last
   `vec_rest
   `vec_nth
   `vec_iterate

   'close
   `in1
   0
   1
   true
   false))

; number of code blocks opened by instructions (default = 0)
(def opened-blocks
  {`exec_dup 1})


;;;;;;;;;
;; Instructions

;; Integer

(defn in1
  "Pushes the input labeled :in1 on the inputs map onto the :exec stack.
  Can't use make-push-instruction, since :input isn't a stack, but a map."
  [state]
  (u/push-to-stack state :exec (:in1 (:input state))))

(defn integer_+
  "Adds the top two integers and leaves result on the integer stack.
  If integer stack has fewer than two elements, noops."
  [state]
  (u/make-push-instruction state +' [:integer :integer] :integer))

(defn integer_-
  "Subtracts the top two integers and leaves result on the integer stack.
  Note: if `first` is the top integer on the stack, and `second` is the
  second, the result pushed to the stack should be (second - first)."
  [state]
  (u/make-push-instruction state -' [:integer :integer] :integer))

(defn integer_*
  "Multiplies the top two integers and leaves result on the integer stack."
  [state]
  (u/make-push-instruction state *' [:integer :integer] :integer))

(defn integer_%
  "This instruction implements 'protected division'.
  In other words, it acts like integer division most of the time, but if the
  denominator is 0, it returns the numerator, to avoid divide-by-zero errors."
  [state]
  (u/make-push-instruction state #(if (zero? %2) %1 (quot %1 %2)) [:integer :integer] :integer))

;; Boolean

(defn boolean_and
  "Takes the top two booleans. If they are both true, push `true` to the
   boolean stack. Otherwise push `false`."
  [state]
  (u/make-push-instruction state #(and %1 %2) [:boolean :boolean] :boolean))

(defn boolean_or
  "Takes the top two booleans. If either is true, push `true` to the
   boolean stack. Otherwise push `false`."
  [state]
  (u/make-push-instruction state #(or %1 %2) [:boolean :boolean] :boolean))

(defn boolean_not
  "Takes the top boolean and negates it."
  [state]
  (u/make-push-instruction state not [:boolean] :boolean))

(defn boolean_=
  "Takes the top two booleans. Pushes `true` if they are the same.
   Otherwise, pushes `false`."
  [state]
  (u/make-push-instruction state = [:boolean :boolean] :boolean))

;; Comparison Operators

(defn integer_=
  "Compares the top two items on the integer stack. If they are equal,
   pushes `true` to the boolean stack. Otherwise, pushes `false`."
  [state]
  (u/make-push-instruction state = [:integer :integer] :boolean))

(defn integer_<
  "Compares the top two items on the integer stack. If the second item on the
   integer stack is less than the first, pushes `true` onto the boolean stack.
   Otherwise, pushes `false` onto the boolean stack."
  [state]
  (u/make-push-instruction state < [:integer :integer] :boolean))

(defn integer_>
  "Compares the top two items on the integer stack. If the second item on the
   integer stack is greater than the first, pushes `true` onto the boolean stack.
   Otherwise, pushes `false` onto the boolean stack."
  [state]
  (u/make-push-instruction state > [:integer :integer] :boolean))

;; Vector instructions (need to make a decision on whether to pop the vector)

(defn vec_count
  "Counts the number of elements in the top item of the vec-integer stack.
   Pushes the result to the integer stack. If the vec-integer stack is empty,
   doesn't do anything."
  [state]
  (if (u/empty-stack? state :vec-integer)
     state
     (u/make-push-instruction state count [:vec-integer] :integer)))

(defn vec_empty?
  "Return true if the top vector is empty, false if it is not. If the vector stack
   is empty, do nothing."
  [state]
  (if (u/empty-stack? state :vec-integer)
    state
    (u/make-push-instruction state empty? [:vec-integer] :boolean)))

(defn vec_first
  "Takes the top vector in the vec-integer stack and pushes the first element of that vector
   onto the integer stack. If the vector stack is empty, or the top vector is empty,
   doesn't do anything."
  [state]
  (if (or (u/empty-stack? state :vec-integer)
          (empty? (u/peek-stack state :vec-integer)))
    state
    (u/make-push-instruction state first [:vec-integer] :integer)))

(defn vec_last
  "Takes the top vector in the vec-integer stack and pushes the last element of that vector
   onto the integer stack. If the vector stack is empty, or the top vector is empty,
   doesn't do anything."
  [state]
  (if (or (u/empty-stack? state :vec-integer)
          (empty? (u/peek-stack state :vec-integer)))
    state
    (u/make-push-instruction state last [:vec-integer] :integer)))

(defn vec_rest
  "Implements a safe `rest` function. Returns a vector with all but
   the first element of the top vector. If the vec-integer stack is
   empty, does nothing."
  [state]
  (cond (u/empty-stack? state :vec-integer)
        state
        (>= 1 (count (u/peek-stack state :vec-integer)))
        ;; Need to avoid clojure automatically casting as a list in this case.
        (u/push-to-stack (u/pop-stack state :vec-integer) :vec-integer [])
        :else
        (u/make-push-instruction state #(apply vector (rest %)) [:vec-integer] :vec-integer)))

(defn vec_nth
  "Implements a safe nth to avoid runtime errors. Take a vector and an integer
   and return the element stored at that index. If either stack is empty, doesn't
   do anything. If the index is negative, uses 0. If the index is positive, returns
   the last element of the vector."
  [state]
  (if (or (u/empty-stack? state :vec-integer)
          (u/empty-stack? state :integer)
          (empty? (u/peek-stack state :vec-integer)))
    state
    (u/make-push-instruction state
                             #(nth %1 (cond
                                        (>= %2 (count %1))
                                        (dec (count %1)) ;; Index too big
                                        (neg? %2) 0      ;; Negative index
                                        :else %2))       ;; Else: valid index
                             [:vec-integer :integer] :integer)))
(defn vec_dup
  [state]
  (if (u/empty-stack? state :vec-integer)
    state
    (u/push-to-stack state :vec-integer (first (:vec-integer state)))))

(defn vec_iterate
  [state]
  (let [args-pop-result (u/get-args-from-stacks state '(:exec :vec-integer))]
    (if (= args-pop-result :not-enough-args)
      state
      (let [popped-state (args-pop-result :state)
            args (args-pop-result :args)
            exec-peek (first args)
            vec-peek (second args)]
        (if (empty? vec-peek)
          popped-state
          (let [element (first vec-peek)
                vec-rest (apply vector (rest vec-peek))]
            ;; Insert a vec-iterate where it needs to be and push the first element
            (u/push-args-to-stacks
             popped-state
             [:vec-integer vec-rest
              :exec exec-peek
              :exec `vec_iterate
              :exec exec-peek
              (u/literal-type-map (type element)) element])))))))

;; Exec instructions

(defn exec_dup
  [state]
  (if (u/empty-stack? state :exec)
    state
    (u/push-to-stack state :exec (first (:exec state)))))

(defn exec_if
  [state]
  (if (or (u/empty-stack? state :boolean)
          (< 2 (count (get state :exec))))
          state
          (let [bool (u/peek-stack state :boolean)
                popped-once (u/pop-stack state :boolean)]
            (if bool
              (u/nth-pop-stack popped-once :exec 1) ;; pop second thing off exec
              (u/pop-stack popped-once :exec)))))   ;; pop first thing off exec