(ns push411.core
  (:require [push411.util :as u]
            [push411.lib :as lib]
            [push411.regression :as reg]
            [push411.increasing :as incr]))

(comment
  "NAME: Anthony Hevia

   core.clj
   
   Impemenets the core GP system as well as the selection and variation
   operators, and other helper functions."
  )


;;;;;;;;;
;; Translation from Plushy genomes to Push programs

(defn translate-plushy-to-push
  "Returns the Push program expressed by the given plushy representation."
  [plushy]
  (let [opener? #(and (vector? %) (= (first %) 'open))] ;; [open <n>] marks opened-blocks
    (loop [push () ;; iteratively build the Push program from the plushy
           plushy (mapcat #(if-let [n (get lib/opened-blocks %)] [% ['open n]] [%]) plushy)]
      (if (empty? plushy)       ;; maybe we're done?
        (if (some opener? push) ;; done with plushy, but unclosed open
          (recur push '(close)) ;; recur with one more close
          push)                 ;; otherwise, really done, return push
        (let [i (first plushy)]
          (if (= i 'close)
            (if (some opener? push) ;; process a close when there's an open
              (recur (let [post-open (reverse (take-while (comp not opener?)
                                                          (reverse push)))
                           open-index (- (count push) (count post-open) 1)
                           num-open (second (nth push open-index))
                           pre-open (take open-index push)]
                       (if (= 1 num-open)
                         (concat pre-open [post-open])
                         (concat pre-open [post-open ['open (dec num-open)]])))
                     (rest plushy))
              (recur push (rest plushy))) ;; unmatched close, ignore
            (recur (concat push [i]) (rest plushy)))))))) ;; anything else


;;;;;;;;;
;; GP

(def mutation-rate 0.1) ;; Defines rates for UMAD to add/delete and for random-genome
(def injection-rate 0.5) ;; Defines the (starting) rate for which injection will continue adding at a given index
(def tournament-size 5) ;; Define the size of sample for tournament selection

(defn make-random-plushy-genome
  "Creates and returns a new plushy genome. Takes a list of instructions and
  a maximum initial Plushy genome size."
  [instructions max-initial-plushy-size]
  (loop [curr-genome '()]
    (if (or (= max-initial-plushy-size (count curr-genome))
            (> mutation-rate (rand)))
      curr-genome
      (let [instruction (rand-nth instructions)]
        (recur (conj curr-genome instruction))))))

(defn initialize-population
  "Initializes a population of random genomes. Takes a population size, a
   list of instructions and a maximum initial Plushy genome size."
  [population-size instructions max-initial-plushy-size]
  (repeatedly population-size #(assoc {} :genome
                                      (make-random-plushy-genome
                                       instructions
                                       max-initial-plushy-size))))

(defn get-best-individual
  "Given a population of individuals, return the one with the lowest total error."
  [population]
  (apply min-key :total-error population))

(defn tournament-selection
  "Selects an individual from the population using a tournament. Returned
  individual will be a parent in the next generation. Can use a fixed
  tournament size."
  [population]
  (let [tournament (take tournament-size (shuffle population))]
    (get-best-individual tournament)
    ))

(defn lexicase-selection
  "Implements lexicase selection. Creates a random ordering of training cases
   and filters out the best individuals according to the cases in that order."
  ([population cases]
   (lexicase-selection population cases (shuffle (range (count cases)))))
  ([population cases case-order]
  (loop [candidates population
         ;; Select a random ordering of the cases.
         case-order case-order]
    (let [i (first case-order)
          ;; Find the lowest error of any remaining candidate on case i
          best (apply min (map #(nth (get % :errors) i) candidates))]
      (cond
        (= 1 (count candidates)) (first candidates)
        (= 1 (count case-order)) (rand-nth candidates)
        ;; Recursive case: filter out anything worse than the best on case i and next case
        :else (recur
               (filter #(>= best (nth (get % :errors) i)) candidates)
               (rest case-order)))))))

(defn semantics-lexicase-selection
  "Lexicase variant. If choosing the second parent, bias the case ordering towards cases
   where the first parent has worst error. Returns a selected individual."
  ([population cases]
   (lexicase-selection population cases))
  ([population cases parent]
   (let [errors (parent :errors)
         grouped-cases (reverse (sort (group-by #(nth errors %) (range (count cases))))) ;; Error->Cases map
         case-order (flatten (map shuffle (vals grouped-cases)))] ;; Shuffle each group
     (lexicase-selection population cases case-order)) ;; Lexicase using that order
   ))

(defn selection
  "Given a selection-method keyword, call the appropriate parent selection method."
  ([selection-method population cases]
   (cond
     (= selection-method :semantics-lexicase)
     (semantics-lexicase-selection population cases)
     (= selection-method :lexicase)
     (lexicase-selection population cases)
     (= selection-method :tournament)
     (tournament-selection population)
     :else :invalid-selection-method))
  ([selection-method population cases parent-a]
  (if (= selection-method :semantics-lexicase)
    (semantics-lexicase-selection population cases parent-a)
    (selection selection-method population cases))))

(defn crossover
  "Crosses over two Plushy genomes (note: not individuals) using uniform crossover.
  Returns child Plushy genome."
  [prog-a prog-b]
  (let [longer-prog (if (> (count prog-a) (count prog-b)) prog-a prog-b)
        size-diff (abs (- (count prog-a) (count prog-b)))
        remaining (take-last size-diff longer-prog)]
    (filter #(not (nil? %)) (concat 
                           (map #(if (> 0.5 (rand)) %1 %2) prog-a prog-b)
                           (if remaining
                             (map #(when (> 0.5 (rand)) %) remaining)
                             '())))))

(defn uniform-mutation
  "Randomly swaps genes from the given program to an instruction from the given
   instruction set with some probability. Returns child Plushy genome."
  [prog instructions]
  (map #(if (> mutation-rate (rand)) (rand-nth instructions) %)
       prog))

(defn uniform-addition
  "Randomly adds new instructions before every instruction (and at the end of
  the Plushy genomes) with some probability. Returns child Plushy genome."
  [prog instructions]
  (concat (if (> mutation-rate (rand)) (list (rand-nth instructions)) '())
          (flatten (map #(if (> mutation-rate (rand)) (list % (rand-nth instructions)) %) prog))))

(defn uniform-deletion
  "Randomly deletes instructions from Plushy genomes at some rate. Returns
   child Plushy genome."
  [prog]
  (random-sample (- 1 mutation-rate) prog))

(defn generate-injection
  "Given a rate to continue injecting, generate a sequence of random instructions
   to inject into a genome (helper for injective variation operators)"
  [instructions chance-to-add]
  (loop [instruction-seq (list (rand-nth instructions))
         chance-to-add chance-to-add]
    (if (> chance-to-add (rand))
      (recur (conj instruction-seq (rand-nth instructions)) (* chance-to-add chance-to-add))
      instruction-seq)))

(defn injective-addition
  "Randomly adds a sequence of instructions before every instruction (and at the
   end of the Plushy genomes) wish some probability. Returns child Plushy genome."
  [prog instructions]
  (concat (if (> mutation-rate (rand))
            (generate-injection instructions injection-rate)
            '())
          (flatten (map #(if (> mutation-rate (rand))
                           (list % (generate-injection instructions injection-rate))
                           %) prog))))

(defn injective-mutation
  "Randomly replaces sequences of genes with a random sequence of genes from the
   given instruction set with some probability. Returns child Plushy genome."
  [prog instructions]
  (flatten (map #(if (> mutation-rate (rand))
                   (generate-injection instructions injection-rate)
                   %) prog)))

(defn select-and-vary
  "Selects parent(s) from population and varies them, returning
  a child individual (note: not program/genome). Chooses which genetic operator
  to use probabilistically. Gives 40% chance to crossover with semantics-lexicase,
  10% to injective-mutation, 25% to uniform-addition, and 25% to uniform-deletion."
  [population instructions training-cases selection-method]
  (let [rand-num (rand)
        parent-a (selection selection-method population training-cases)
        genome-a (get parent-a :genome)]
    (cond
      (< 0.6 rand-num) {:genome (crossover genome-a
                                           (get (selection selection-method population training-cases parent-a) :genome))}
      (< 0.5 rand-num) {:genome (uniform-mutation genome-a instructions)}
      (< 0.25 rand-num) {:genome (uniform-addition genome-a instructions)}
      :else {:genome (uniform-deletion genome-a)})))

(defn report
  "Reports information on the population and the best individual each generation."
  [generation population best-individual]
  (let [error-vectors (map #(get % :errors) population)
        num-cases (count (first error-vectors))
        num-distinct-error-vectors (count (distinct error-vectors))
        errors-by-case (map (fn [n] (map #(nth % n)
                                         error-vectors))
                            (range num-cases))
        lowest-error-per-case (map #(apply min %) errors-by-case)]
    
    (println "-------------------------------------------------------")
    (println "               Report for Generation" generation)
    (println "-------------------------------------------------------")
    (println "Best program:" (get best-individual :program))
    (println "Best individual genome size:" (count (get best-individual :genome)))
    (println "Best total error:" (get best-individual :total-error))
    (println "Best errors:" (get best-individual :errors))
    (println "Lowest error per case:" lowest-error-per-case)
    (println "Proportion of distinct error vectors:" num-distinct-error-vectors "/" (count population))))


(defn push-gp
  "Main GP loop. Initializes the population, and then repeatedly
  generates and evaluates new populations. Stops if it finds an
  individual with 0 error (and should return :SUCCESS, or if it
  exceeds the maximum generations (and should return nil). Should print
  report each generation.
  --
  The only argument should be a map containing the core parameters to
  push-gp. The format given below will decompose this map into individual
  arguments. These arguments should include:
   - population-size
   - max-generations
   - error-function
   - training-cases
   - instructions (a list of instructions)
   - max-initial-plushy-size (max size of randomly generated Plushy genomes)"
  [{:keys [population-size max-generations training-cases error-function instructions max-initial-plushy-size selection]
    :as argmap}]
  (loop [population (initialize-population population-size
                                           instructions
                                           max-initial-plushy-size)
         generation 0]
    ;; Evaluate the population (in parallel) and retrieve the best individual
    (let [evaluated-population (pmap error-function population)
          best-individual (get-best-individual evaluated-population)]
      (report generation evaluated-population best-individual)
      (cond
        (zero? (get best-individual :total-error)) best-individual
        (= generation max-generations) nil
        ;; Recursive case: get the population for the next generation
        :else (let [new-population (repeatedly population-size 
                                               #(select-and-vary evaluated-population
                                                                 instructions
                                                                 training-cases
                                                                 selection))]
                (recur new-population (inc generation)))))))


;;;;;;;;;;
;; The error function
;; The functions below are specific to a particular problem.
;; A different problem would require replacing these functions.
;; Problem: f(x) = x^3 + x + 3

(defn regression-error-function
  "Takes an individual and evaluates it on some training cases.
  This will need to translate each individual's Plushy genome into a Push
  program before executing the Push program (see translate-plushy-to-push).
  For each training case,
  runs program with the input set to :in1 in the :input map part of the Push state.
  Then, the output is the integer on top of the integer stack in the Push state
  returned by the interpreter. Computes each error by comparing output of
  the program to the correct output.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors. You may also want to set
  :program to be the Push program translated from the Plushy genome, though
  this isn't mandatory.
  Note: You must consider what to do if the program doesn't leave anything
  on the integer stack."
  [individual]
  (let [ ;; Bindings responsible for translating, running, and calculating errors 
        genome (get individual :genome)
        program (translate-plushy-to-push genome)
        ;; Programs will run on an empty push state with the inputs from training set
        start-states (pmap #(assoc-in u/empty-push-state [:input :in1] %)
                          reg/inputs)
        ;; Gets state of all stacks after running, then we extract the top integer
        output-states (pmap #(u/interpret-push-program program %) start-states)
        program-outputs (pmap #(u/peek-stack % :integer) output-states)
        ;; Error is absolute difference between expected and received result per case
        ;; If :no-stack-item we give the program infinite error as penalty
        errors (apply vector (pmap #(if (= :no-stack-item %1) ##Inf (abs (- %1 %2)))
                                   program-outputs reg/expected-outputs))
        total-error (apply +' errors)]
    ;; program has run on training data, now just need to store results in individual
    (assoc individual :program program
                      :errors errors
                      :total-error total-error)))

(defn increasing-error-function
  "Takes an individual and evaluates it on some training cases.
  Returns the individual with :errors set to the list of errors on each case,
  and :total-error set to the sum of the errors. Sets
  :program to be the Push program translated from the Plushy genome."
  [individual]
  (let [;; Bindings responsible for translating, running, and calculating errors 
        genome (get individual :genome)
        program (translate-plushy-to-push genome)
        ;; Programs will run on an empty push state with the inputs from training set
        start-states (pmap #(assoc-in u/empty-push-state [:input :in1] %)
                           incr/inputs)
        ;; Gets state of all stacks after running, then we extract the top boolean
        output-states (pmap #(u/interpret-push-program program %) start-states)
        program-outputs (pmap #(u/peek-stack % :boolean) output-states)
        ;; Error = 0 if the top boolean matches expected output, 1 if not
        ;; If :no-stack-item we give the program infinite error as penalty
        errors (apply vector (pmap #(if (= :no-stack-item %1)
                                      ##Inf
                                      (if (= %1 %2) 0 1))
                                   program-outputs incr/expected-outputs))
        total-error (apply +' errors)]
    ;; program has run on training data, now just need to store results in individual
    (assoc individual :program program
           :errors errors
           :total-error total-error)))

(defn increasing-generalized?
  [individual]
  (let [;; Bindings responsible for translating, running, and calculating errors 
        genome (get individual :genome)
        program (translate-plushy-to-push genome)
          ;; Programs will run on an empty push state with the inputs from test set
        start-states (pmap #(assoc-in u/empty-push-state [:input :in1] %)
                           incr/test-set-inputs)
          ;; Gets state of all stacks after running, then we extract the top boolean
        output-states (pmap #(u/interpret-push-program program %) start-states)
        program-outputs (pmap #(u/peek-stack % :boolean) output-states)
          ;; Error = 0 if the top boolean matches expected output, 1 if not
        ;; If :no-stack-item we give the program infinite error as penalty
        errors (apply vector (pmap #(if (= :no-stack-item %1)
                                      ##Inf
                                      (if (= %1 %2) 0 1))
                                   program-outputs incr/test-set-expected-outputs))]
      ;; Report accuracy on test data and return whether or not solution generalized
    (println "TEST SET ACCURACY:" (double (/ (count (filter zero? errors)) (count errors))))
    (every? zero? errors)))

;;;;;;;;;;
;; The main function call
;; You can call this in a REPL, or alternatively from the command line
;; by running:
;;   clj -X push411.core/main
;; Additionally, if you want to pass command line arguments as a map to args,
;; you can run something like:
;;   clj -X push411.core/main "{:selection :lexicase}"

(defn main
  "Runs push-gp, giving it a map of arguments. Reports on the solution (or failure)"
  ([] (main {}))
  ([args]
        ;; Get any arguments provided from main, or give them a default value.
        ;; NOTE: For now, only generations, population size, plushy size, and selection
        ;;       are expected to be provided by user.
   (let [argmap {:instructions (get args :instructions lib/default-instructions)
                 :error-function (get args :error-function increasing-error-function)
                 :training-cases (get args :training-cases incr/cases)
                 :max-generations (get args :max-generations 500)
                 :population-size (get args :population-size 200)
                 :max-initial-plushy-size (get args :max-initial-plushy-size 50)
                 :selection (get args :selection :semantics-lexicase)}
         solution (push-gp argmap)]
     (if (some? solution)
       (do
         (println "SOLUTION FOUND:")
         (println solution)
         (println (if (increasing-generalized? solution)
                    "INITIAL SOLUTION GENERALIZED"
                    "INITIAL SOLUTION FAILED TO GENERALIZE"))
         (println "RUNNING AUTOMATIC SIMPLIFICATION...")
         (let [simplified-solution (u/first-choice-automatic-simplifier solution increasing-error-function 5000)]
           (println "SIMPLIFIED SOLUTION:")
           (println simplified-solution)
           (println (if (increasing-generalized? simplified-solution)
                      "SIMPLIFIED SOLUTION GENERALIZED"
                      "SIMPLIFIED SOLUTION FAILED TO GENERALIZE"))))
       (println "FAILED")))))

(comment
  
  ;; No arguments
  (main)

  ;; Use args as a map
  (main {:selection :semantics-lexicase})
  (main {:selection :lexicase})
  (main {:population-size 300 :selection :semantics-lexicase})
  )