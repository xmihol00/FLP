import numpy as np

max_nodes = ord('Z') - ord('A') + 1

for i in range(2, np.ceil(np.sqrt(max_nodes)).astype(int)):
    for j in range(2, max_nodes + 1):
        if i * j <= max_nodes:
            mesh = np.array([chr(x) for x in range(ord('A'), ord('A') + i * j)]).reshape(i, j)
            with open(f"{i}-{j}_mesh.txt", "w") as f:
                for k in range(mesh.shape[0] - 1):
                    for l in range(mesh.shape[1] - 1):
                        f.write(f"{mesh[k, l]} {mesh[k, l + 1]}\n")
                        f.write(f"{mesh[k, l]} {mesh[k + 1, l]}\n")