
start_char = ord('A')
for end_char in range(ord('A'), ord('Z') + 1):
    nodes = list(range(start_char, end_char + 1))
    with open(f"{chr(start_char)}-{chr(end_char)}_fully_connected.txt", "w") as f:
        for node_a in nodes:
            for node_b in nodes:
                    f.write(f"{chr(node_a)} {chr(node_b)}\n")
                    