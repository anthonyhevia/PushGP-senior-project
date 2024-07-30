#!/usr/bin/python3

"""
Uses scrape_run to scrape each subdirectory of given directory.
Uses csv printing and not verbose by default
"""

import sys, os
import scrape_run as scraper
import csv

parent_dir = sys.argv[1]

param_dirs = os.listdir(parent_dir)
param_dirs.sort()

# print(param_dirs)

for param in param_dirs: # Will go through each folder in results directory
    # Set up info on which subdirectory of the results folder we are in
    full = parent_dir + "/" + param
    runs = os.listdir(full)
    runs.sort()
    num_runs = len(runs)

    # Keep track of these metrics to report
    training_successes = 0
    initial_generalizations = 0
    simplified_generalizations = 0
    avg_initial_test_accuracy = 0
    avg_simplified_test_accuracy = 0
    avg_error_diversity_per_generation = [0 for _ in range(500)]
    runs_reaching_each_generation = [0 for _ in range(500)]
    generations = [0 for _ in range(num_runs)]
    
    # Only check actual text files
    runs = filter( lambda x: x.endswith(".txt"), runs)
    for i, run in enumerate(runs): # Will go through each run with current parameters
        run_dir = full + "/" + run
        # Returns a dictionary whose keys are desired metrics
        run_summary = scraper.scrape_run(run_dir)
        # Update each metric
        generations[i] = run_summary["generation"]
        training_successes += 1 if run_summary["solution?"] else 0
        initial_generalizations += 1 if run_summary["initial generalized?"] else 0
        simplified_generalizations += 1 if run_summary["simplified generalized?"] else 0
        avg_initial_test_accuracy += run_summary["initial accuracy"]
        avg_simplified_test_accuracy += run_summary["simplified accuracy"]
        for j in range(generations[i]):
            avg_error_diversity_per_generation[j] += run_summary["distinct errors"][j]
            runs_reaching_each_generation[j] += 1
    max_generations = 500
    for i, n in enumerate(runs_reaching_each_generation):
        if n == 0:
            max_generations = i
            break
    avg_error_diversity_per_generation = avg_error_diversity_per_generation[:max_generations]
    for i in range(max_generations):
        avg_error_diversity_per_generation[i] = format(avg_error_diversity_per_generation[i] / runs_reaching_each_generation[i], '.4f')            
    avg_initial_test_accuracy /= training_successes
    avg_simplified_test_accuracy /= training_successes
    # Report metrics for the current subdirectory (or parameter setting)
    print(param)
    print("Training successes:", training_successes)
    print("Pre-simplification generalized solutions:", initial_generalizations)
    print("Post-simplification generalized solutions:", simplified_generalizations)
    print("Average test accuracy of solutions pre-simplification:", avg_initial_test_accuracy)
    print("Average test accuracy of solutions post-simplification:", avg_simplified_test_accuracy)
    print("Average proportion of distinct errors per generation:", avg_error_diversity_per_generation)
    print("average length of run:", sum(generations) / len(generations))
    print()
