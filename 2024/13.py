from pathlib import Path

machine_strs = Path('13input').read_text().strip().split('\n\n')

def split_ab(ab):
    x, y = ab.split(': ')[1][len('X+'):].split(', ')
    return int(x), int(y[2:])

def solution(m):
    a_str, b_str, p_str = m.split('\n')
    ax, ay = split_ab(a_str)
    bx, by = split_ab(b_str)
    px, py = split_ab(p_str)

    for a in range(100):
        for b in range(100):
            if ax*a + bx*b == px and ay*a + by*b == py:
                return a*3 + b

tokens = 0

for m in machine_strs:
    sol = solution(m)
    if sol:
        tokens += sol

print('Part 1:', tokens)
