from pathlib import Path
from functools import cache

initial_values_str, gates_str = Path('24input').read_text().split('\n\n')

initial_values = {l.split(': ')[0]: int(l.split(': ')[1]) for l in initial_values_str.splitlines()}

gates = {}
for s in gates_str.splitlines():
    op_str, gate = s.split(' -> ')
    gates[gate] = op_str.split(' ')

@cache
def gate_value(g):
    if g in initial_values:
        return initial_values[g]

    op1, op, op2 = gates[g]
    v1 = gate_value(op1)
    v2 = gate_value(op2)

    match op:
        case 'AND':
            return v1 & v2
        case 'OR':
            return v1 | v2
        case 'XOR':
            return v1 ^ v2

z_gates = sorted([g for g in gates if 'z' in g])
bits = sum(gate_value(z) << i for i, z in enumerate(z_gates))

print('Part 1:', bits)

