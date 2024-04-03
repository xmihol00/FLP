import argparse
import re
import termcolor

def create_graphs(solutions):
    graphs = []
    invalid_graphs = []

    if solutions[0]: # if the solutions aren't empty
        for solution in solutions:
            graph = []
            solution_dict = {splitted[0]: splitted[1] for splitted in [s.split('-') for s in solution]}
            first = solution[0].split('-')
            graph.append(first[0])
            current = first[1]
            try:
                for _ in range(len(solution) - 1):
                    graph.append(current)
                    current = solution_dict[current]
                if current == first[0]:
                    graphs.append(graph)
                else:
                    invalid_graphs.append(solution)
            except:
                invalid_graphs.append(solution)
                continue

    return graphs, invalid_graphs

parser = argparse.ArgumentParser(description='Compare two solutions')
parser.add_argument('ground_truth_file', type=str, help='File containing the ground truth solution with a specific format.')
parser.add_argument('solution_file', type=str, help='File containing the solution to be compared with the ground truth containing only the test output.')

args = parser.parse_args()

with open(args.ground_truth_file, 'r') as f:
    ground_truth = f.read()

with open(args.solution_file, 'r') as f:
    tested_solution = f.read()

gt_solution = re.findall(r"print solution:\n(.*)\n", ground_truth, re.DOTALL)
gt_solution = gt_solution[0] if gt_solution else ""
gt_solutions = gt_solution.split('\n')
gt_solutions = [solution.split() for solution in gt_solutions]
gt_solutions, invalid_gt_solutions = create_graphs(gt_solutions)
if invalid_gt_solutions:
    print(termcolor.colored("FAILED", "yellow"))
    print("Not parsable ground truth solutions:")
    for tested_solution in invalid_gt_solutions:
        print(tested_solution)
    exit(2)
rotate_char = gt_solutions[-1][-1] if gt_solutions and gt_solutions[-1] else None

if rotate_char is not None:
    for i in range(len(gt_solutions)):
        index = gt_solutions[i].index(rotate_char)
        gt_solutions[i] = gt_solutions[i][index:] + gt_solutions[i][:index]

mirrored_gt_solutions = []
for gt_solution in gt_solutions:
    mirrored_gt_solutions.append(gt_solution[1:])
    mirrored_gt_solutions.append(gt_solution[1:][::-1])

tested_solutions = tested_solution.strip().split('\n')
tested_solutions = [solution.split() for solution in tested_solutions]
tested_solutions, invalid_tested_solutions = create_graphs(tested_solutions)
if invalid_tested_solutions:
    print(termcolor.colored("FAILED", "red"))
    print("Not parsable solutions:")
    for tested_solution in invalid_tested_solutions:
        print(tested_solution)
    exit(1)
if rotate_char is not None and tested_solutions[0]:
    for i in range(len(tested_solutions)):
        index = tested_solutions[i].index(rotate_char)
        tested_solutions[i] = tested_solutions[i][index:] + tested_solutions[i][:index]

mirrored_tested_solutions = []
for tested_solution in tested_solutions:
    mirrored_tested_solutions.append(tested_solution[1:])
    mirrored_tested_solutions.append(tested_solution[1:][::-1])

set_of_gt_tuples = set(tuple(solution) for solution in mirrored_gt_solutions)
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
