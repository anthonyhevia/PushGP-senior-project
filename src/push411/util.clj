(ns push411.util)

(comment
  "NAME: Anthony Hevia

   Utilities
   Implements necessary stack operations for Push and
   helper functions for creating push instructions
   ")

;; Maps types in clojure to Push stacks. Used in Push interpreter.
;; NOTE: maps any vector to the stack :vec-integer.
;;       Should change if more vector types are added.
(def literal-type-map
  {Long :integer
   Double :float
   String :string
   Boolean :boolean
   clojure.lang.PersistentVector :vec-integer})

(def empty-push-state
  {:exec '()
   :integer '()
   :string '()
   :boolean '()
   :vec-integer '()
   :input {}})

;;;;;;;;;;
;; Stack instructions

(defn push-to-stack
  "Pushes item onto stack in state, returning the resulting state."
  [state stack item]
  (assoc state stack (cons item (get state stack))))

(defn pop-stack
  "Removes top <num-pops> items of stack, returning the resulting state."
  ([state stack]
   (pop-stack state stack 1)) ;; By default, pops 1 item
  ([state stack num-pops]
   (assoc state stack (nthrest (get state stack) num-pops))))

(defn nth-pop-stack
  "Removes the item at index n of the stack, returning the resulting state."
  [state stack index]
  (let [old-stack (get state stack)
        new-stack (concat (take index old-stack) (nthrest old-stack (inc index)))]
    (assoc state stack new-stack)
    ))

(defn empty-stack?
  "Returns true if the stack is empty in state."
  [state stack]
  (empty? (get state stack)))

(defn peek-stack
  "Returns top item on a stack. If stack is empty, returns :no-stack-item"
  [state stack]
  (if (empty-stack? state stack)
    :no-stack-item
    (first (get state stack))))

(defn push-code-block
  "Pushes a code block onto the exec stack (helper for interpreter)"
  [state code-block]
  (loop
   [curr-state state
    remaining-block code-block
    steps 0]
    (if (empty? remaining-block)
      curr-state
      (recur (push-to-stack curr-state :exec (last remaining-block))
             (butlast remaining-block)
             (inc steps)))))

(defn push-args-to-stacks
  "Pushes a list of elements to their stacks. Takes a sequence of bindings (stack, item)
   so it knows which stack to push each item to. Note: pushes in order of items give, so
   the last item pushed will be at the top of its respective stack."
  [state bindings]
  (loop [state state
         bindings bindings]
    (if (empty? bindings)
      state
      (recur (push-to-stack state (first bindings) (second bindings))
             (nthrest bindings 2))))) ;; Remove the first binding from this vector

(defn get-args-from-stacks
  "Takes a state and a list of stacks to take args from. If there are enough args
  on each of the desired stacks, returns a map with keys {:state :args}, where
  :state is the new state with args popped, and :args is a list of args from
  the stacks. If there aren't enough args on the stacks, returns :not-enough-args."
  [state stacks]
  (loop [state state
         stacks (reverse stacks)
         args '()]
    (if (empty? stacks)
      {:state state :args args}
      (let [stack (first stacks)]
        (if (empty-stack? state stack)
          :not-enough-args
          (recur (pop-stack state stack)
                 (rest stacks)
                 (conj args (peek-stack state stack))))))))

(defn make-push-instruction
  "A utility function for making Push instructions. Takes a state, the function
  to apply to the args, the stacks to take the args from, and the stack to return
  the result to. Applies the function to the args (taken from the stacks) and pushes
  the return value onto return-stack in the resulting state."
  [state function arg-stacks return-stack]
  (let [args-pop-result (get-args-from-stacks state arg-stacks)]
    (if (= args-pop-result :not-enough-args)
      state
      (let [result (apply function (:args args-pop-result))
            new-state (:state args-pop-result)]
        (push-to-stack new-state return-stack result)))))

;;;;;;;;;
;; Interpreter

(defn interpret-one-step
  "Helper function for interpret-push-program.
  Takes a Push state and executes the next instruction on the exec stack,
  or if the next element is a literal, pushes it onto the correct stack.
  Or, if the next element is a nested list, needs to unwrap that list onto
  the exec stack.
  Returns the new Push state."
  [push-state]
  (let [element (peek-stack push-state :exec)
        new-push-state (pop-stack push-state :exec)]
    (cond
      (symbol? element) ;; Push instructions = symbols: get args and eval
      ((eval element) new-push-state)
      (list? element) ;; Code blocks = lists: push to exec stack (last->first)
      (push-code-block new-push-state element)
      :else ;; Everything else = literal: push to appropriate type stack
      (push-to-stack new-push-state (get literal-type-map (type element)) element))))

(defn interpret-push-program
  "Runs the given program starting with the stacks in start-state. Continues
  until the exec stack is empty. Returns the state of the stacks after the
  program finishes executing.
   To avoid infinite execution, you will need to enforce some maximum number
  of interpreter steps before terminating the program. You can choose this limit."
  [program start-state]
  (loop [curr-state (push-code-block start-state program)
         curr-step 0]
    (if (or (= 200 curr-step) (empty-stack? curr-state :exec))
      curr-state ;; Base case: end of program or maximum steps reached
      (recur     ;; Recursive case: interpret one step and recurse
       (interpret-one-step curr-state)
       (inc curr-step)))))

;;;;;;;;
;; Automatic Simplification

(defn first-choice-automatic-simplifier
  "Given a solution program, perform first-choice hill-climbing to reduce
   the program to a smaller program that behaves identically on the given
   test cases."
  [prog error-function step-limit]
  (loop [curr-prog prog
         curr-step 0]
    (let [curr-genome (get curr-prog :genome)
          genome-size (count curr-genome)
          to-delete (apply hash-set
                           (take (inc (rand-int 4)) (shuffle (range genome-size))))
          new-genome (filter some? ;; Remove genes at indices in to-delete list
                             (map (fn [gene i] (if (some? (get to-delete i))
                                                 nil
                                                 gene)) curr-genome (range genome-size)))
          new-prog (error-function {:genome new-genome})
          curr-errors (get curr-prog :errors)
          new-errors (get new-prog :errors)]
      (cond
        (= curr-step step-limit)
        curr-prog
        (every? true? (map #(<= %1 %2) new-errors curr-errors))
        (recur new-prog (inc curr-step))
        :else (recur curr-prog (inc curr-step))))))