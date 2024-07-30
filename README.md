# push411

## Usage

There are two ways to run the main PushGP function:

1. Load `core.clj` into the interpreter, and then run `(main {:problem <problem>})`.
2. From the command line, run `clj -X push411.core/main "{:problem <problem>}"`.

## Required Arguments

1. :problem - the problem to evolve a solution towards:
   
    :regression - symbolic regression problem
   
    :increasing - boolean problem (given a vector of integers, is it sorted in non-decreasing order?)

## Optional Arguments

The following are optional parameters:

1. :max-generations - The maximum number of generations to run before terminating.
2. :population-size - The size of the GP population
3. :max-initial-plushy-size - The largest possible size for a program generated at initialization
4. :selection - the method to be used for parent selection:
   
    :lexicase
   
    :semantics-lexicase
   
    :tournament

By default, the system will run for 500 generations on a population of size 200, a max initial plushy size of 50, and will use lexicase selection. The user MUST specify the problem. For future use, a goal is to make the system compatible with more optional parameters but currently those are not expected to be supplied by the user.

A few examples on running main with these parameters:

From the REPL:

`(main {:problem :regression :selection :lexicase})`

`(main {:problem :increasing :max-generations 200 :population-size 100})`

From the command line:

`clj -X push411.core/main "{:problem :regression :selection :lexicase}"`

`clj -X push411.core/main "{:problem :increasing :max-generations 200 :population-size 100}"`
