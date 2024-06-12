#!/usr/bin/python3

"""
Scrapes a given run file for:
    - best program size per generation
    - best program total error per generation
    - distinct errors per generation
    - success/fail on training data
    - if success:
        - generation of solution found
        - accuracy on test data
        - success/fail on test data
"""

import sys, os

def scrape_run(directory):

    # print("scrape run called on {0}".format(directory))

    sizes_of_best = []
    errors_of_best = []
    distinct_errors = []
    training_success = False
    initial_generalized = False
    initial_test_accuracy = 0
    simplified_generalized = False
    simplified_test_accuracy = 0
    current_generation = -1
    with open(directory, "r") as f:

        for line in f.readlines():

            if line.lstrip().startswith("Report for Generation"):
                current_generation += 1
                # print("current generation:", current_generation)

            if line.startswith("Best individual genome size"):
                size = int(line.split()[-1])
                # print("size of genome: {0}".format(size))
                sizes_of_best.append(size)

            if line.startswith("Best total error"):
                error = int(line.split()[-1])
                # print("error of best program:", error)
                errors_of_best.append(error)

            if line.startswith("Proportion of distinct error vectors"):
                split_line = line.split()
                distinct = int(split_line[-3]) / int(split_line[-1])
                # print("distinct errors", distinct)
                distinct_errors.append(distinct)

            if line.startswith("SOLUTION FOUND"):
                training_success = True

            if line.startswith("INITIAL SOLUTION GENERALIZED"):
                initial_generalized = True
                # print("INITIAL GENERALIZED!")

            if line.startswith("SIMPLIFIED SOLUTION GENERALIZED"):
                simplified_generalized = True
                # print("SIMPLIFIED GENERALIZED!")

            if line.startswith("TEST SET ACCURACY"):
                if (initial_test_accuracy == 0):
                    initial_test_accuracy = float(line.split()[-1])
                    # print(initial_test_accuracy)
                else:
                    simplified_test_accuracy = float(line.split()[-1])
                    # print(simplified_test_accuracy)
                
    return {"sizes of best" : sizes_of_best,
            "errors of best" : errors_of_best,
            "distinct errors" : distinct_errors,
            "solution?" : training_success,
            "initial generalized?" : initial_generalized,
            "initial accuracy" : initial_test_accuracy,
            "simplified generalized?" : simplified_generalized,
            "simplified accuracy" : simplified_test_accuracy,
            "generation" : current_generation
            }

    


