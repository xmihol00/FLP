import argparse
import re
import termcolor

parser = argparse.ArgumentParser(description='Compare two solutions')
parser.add_argument('ground_truth_file', type=str, help='File containing the ground truth solution with a specific format.')
parser.add_argument('solution_file', type=str, help='File containing the solution to be compared with the ground truth containing only the test output.')

args = parser.parse_args()

with open(args.ground_truth_file, 'r') as f:
    ground_truth = f.read()

with open(args.solution_file, 'r') as f:
    tested_solution = f.read()

gt_solution = re.findall(r"test solution:\n(.*)\n\n", ground_truth, re.DOTALL)
gt_solution = gt_solution[0] if gt_solution else ""
gt_solutions = gt_solution.split('\n')
gt_solutions = [solution.split() for solution in gt_solutions]
rotate_char = gt_solutions[-1][-1] if gt_solutions and gt_solutions[-1] else None

if rotate_char is not None:
    for i in range(len(gt_solutions)):
        index = gt_solutions[i].index(rotate_char)
        gt_solutions[i] = gt_solutions[i][index:] + gt_solutions[i][:index]

gt_mirrored_solutions = []
for gt_solution in gt_solutions:
    gt_mirrored_solutions.append(gt_solution[1:])
    gt_mirrored_solutions.append(gt_solution[1:][::-1])

tested_solutions = tested_solution.strip().split('\n')
tested_solutions = [solution.split() for solution in tested_solutions]
if rotate_char is not None and tested_solutions[0]:
    for i in range(len(tested_solutions)):
        index = tested_solutions[i].index(rotate_char)
        tested_solutions[i] = tested_solutions[i][index:] + tested_solutions[i][:index]

mirrored_tested_solutions = []
for tested_solution in tested_solutions:
    mirrored_tested_solutions.append(tested_solution[1:])
    mirrored_tested_solutions.append(tested_solution[1:][::-1])

set_of_gt_tuples = set(tuple(solution) for solution in gt_mirrored_solutions)
set_of_tested_tuples = set(tuple(solution) for solution in mirrored_tested_solutions)
if len(mirrored_tested_solutions) == 2 and len(mirrored_tested_solutions[0]) == 1: # special case for solution with length 1
    mirrored_tested_solutions = mirrored_tested_solutions[:1]
set_size_vs_list_size = len(set_of_tested_tuples) == len(mirrored_tested_solutions) or set_of_tested_tuples == {()}

gt_diff = set_of_gt_tuples - set_of_tested_tuples
diff = set_of_tested_tuples - set_of_gt_tuples

if len(gt_diff) == 0 and len(diff) == 0 and set_size_vs_list_size:
    print(termcolor.colored("PASSED", "green"))
    exit(0)
elif not set_size_vs_list_size:
    print(termcolor.colored("FAILED", "red"))
    print("Duplicate solutions found in the output file:")
    seen = set()
    for tested_solution in mirrored_tested_solutions:
        if tuple(tested_solution) in seen:
            if rotate_char is not None:
                tested_solution.insert(0, rotate_char)
            print(tested_solution)
        else:
            seen.add(tuple(tested_solution))
    exit(1)
else:
    print(termcolor.colored("FAILED", "red"))
    print("Extra invalid solutions:")
    for tested_solution in diff:
        if tested_solution:
            tested_solution = list(tested_solution)
            if rotate_char is not None:
                tested_solution.insert(0, rotate_char)
            print(tested_solution)
    print("Missing valid solutions:")
    for tested_solution in gt_diff:
        tested_solution = list(tested_solution)
        if rotate_char is not None:
            tested_solution.insert(0, rotate_char)
        print(tested_solution)
    exit(1)
