import numpy as np

max_nodes = ord('Z') - ord('A') + 1

for i in range(2, np.ceil(np.sqrt(max_nodes)).astype(int)):
    for j in range(i, max_nodes + 1):
        if i * j <= max_nodes:
            mesh = np.array([chr(x) for x in range(ord('A'), ord('A') + i * j)]).reshape(i, j)
            filename = f"{i}-{j}_2D_mesh.txt" if j > 9 else f"{i}-0{j}_2D_mesh.txt"
            with open(filename, "w") as f:
                for k in range(i):
                    for l in range(j):
                        if k + 1 < i:
                            f.write(f"{mesh[k, l]} {mesh[k + 1, l]}\n")
                        if l + 1 < j:
                            f.write(f"{mesh[k, l]} {mesh[k, l + 1]}\n")