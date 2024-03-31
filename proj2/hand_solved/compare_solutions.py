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
    solution = f.read()

gt_solution = re.findall(r'test solution:\n(.*)\n\n', ground_truth, re.DOTALL)
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

solutions = solution.strip().split('\n')
solutions = [solution.split() for solution in solutions]
if rotate_char is not None:
    for i in range(len(solutions)):
        index = solutions[i].index(rotate_char)
        solutions[i] = solutions[i][index:] + solutions[i][:index]

mirrored_solutions = []
for solution in solutions:
    mirrored_solutions.append(solution[1:])
    mirrored_solutions.append(solution[1:][::-1])

gt_set_of_tuples = set(tuple(solution) for solution in gt_mirrored_solutions)
set_of_tuples = set(tuple(solution) for solution in mirrored_solutions)
if len(mirrored_solutions) == 2 and len(mirrored_solutions[0]) == 1: # special case for solution with length 1
    mirrored_solutions = mirrored_solutions[:1]
set_size_vs_list_size = len(set_of_tuples) == len(mirrored_solutions) or set_of_tuples == {()}

gt_diff = gt_set_of_tuples - set_of_tuples
diff = set_of_tuples - gt_set_of_tuples

if len(gt_diff) == 0 and len(diff) == 0 and set_size_vs_list_size:
    print(termcolor.colored("PASSED", "green"))
elif not set_size_vs_list_size:
    print(termcolor.colored("FAILED", "red"))
    print("Duplicate solutions found in the output file:")
    seen = set()
    for solution in mirrored_solutions:
        if tuple(solution) in seen:
            solution.insert(0, rotate_char)
            print(solution)
        else:
            seen.add(tuple(solution))
else:
    print(set_of_tuples)
    print(gt_set_of_tuples)
    print(termcolor.colored("FAILED", "red"))
    print("Extra invalid solutions:")
    for solution in diff:
        solution = list(solution)
        solution.insert(0, rotate_char)
        print(solution)
    print("Missing valid solutions:")
    for solution in gt_diff:
        solution = list(solution)
        solution.insert(0, rotate_char)
        print(solution)
