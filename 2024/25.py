from pathlib import Path

inputs = Path('25input').read_text().split('\n\n')

keys = []
locks = []

for i in inputs:
    transposed = [''.join(s) for s in zip(*i.splitlines())]

    if transposed[0][0] == '#':
        keys.append([len(s.rstrip('.')) for s in transposed])
    else:
        locks.append([len(s.rstrip('#')) for s in transposed])

def key_fits(k, l):
    for i in range(5):
        if k[i] > l[i]:
            return False
    return True

count = 0
for k in keys:
    for l in locks:
        if key_fits(k, l):
            count += 1

print('Part 1:', count)
