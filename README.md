# push411

This is the template for a PushGP system for Hamilton's CS 411: Genetic Programming class.

## Usage

There are two ways to run the main PushGP function:

1. Load `core.clj` into the interpreter, and then run `(main)`.
2. From the command line, run `clj -X push411.core/main`.

## Optional Arguments

The following are optional parameters:

1. :max-generations - The maximum number of generations to run before terminating.
2. :population-size - The size of the GP population
3. :max-initial-plushy-size - The largest possible size for a program generated at initialization
4. :selection - the method to be used for parent selection:
    :lexicase
    :semantics-lexicase
    :tournament

By default, the system will run for 500 generations on a population of size 200, a max initial plushy size of 50, and will use semantics lexicase. For future use, a goal is to make the system compatible with more optional parameters (such as problem description, error function, etc.) but currently those are not expected to be supplied by the user.

A few examples on running main with these parameters:

From the interpreter:

`(main {:selection :lexicase})`

`(main {:max-generations 200 :population-size 100})`

From the command line:

`clj -X push411.core/main "{:selection :lexicase}"`

`clj -X push411.core/main "{:max-generations 200 :population-size 100}"`
