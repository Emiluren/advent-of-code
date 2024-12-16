import numpy as np
from pathlib import Path

machine_strs = Path('13input').read_text().split('\n\n')

def split_ab(ab):
    x, y = ab.split(': ')[1][len('X+'):].split(', ')
    return int(x), int(y[2:])

for m in machine_strs:
    a_str, b_str, p_str = m.split('\n')
    ax, ay = split_ab(a_str)
    bx, by = split_ab(b_str)
    px, py = split_ab(p_str)

    print(repr(np.linalg.solve(np.array([[ax, bx], [ay, by]]), np.array([px, py]))))
    break

