
start_char = ord('A')
for end_char in range(ord('A'), ord('Z') + 1):
    with open(f"{chr(start_char)}-{chr(end_char)}_circle.txt", "w") as f:
        for node_a, node_b in zip(range(start_char, end_char), range(start_char + 1, end_char + 1)):
            f.write(f"{chr(node_a)} {chr(node_b)}\n")
        f.write(f"{chr(end_char)} {chr(start_char)}\n")
    