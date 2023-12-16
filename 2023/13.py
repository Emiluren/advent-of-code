with open('13input') as f:
    patterns = [[l for l in p.split('\n')] for p in f.read().strip().split('\n\n')]

def find_mirror_point(rows):
    height = len(rows)
    for i in range(1, height):
        slice_height = min(i, height-i)
        top = rows[i-slice_height:i]
        bottom = list(reversed(rows[i:i+slice_height]))
        if top == bottom:
            return i

def print_pattern(p):
    print('rows')
    cols = list(map(''.join, zip(*p)))
    for r in p:
        print(r)
    print()
    print('cols', cols)
    for c in cols:
        print(c)
    print()


s = 0
for i, p in enumerate(patterns):
    mp = find_mirror_point(p)
    if mp is not None:
        s += 100 * mp
    else:
        cols = list(map(''.join, zip(*p)))
        mp = find_mirror_point(cols)
        s += mp

print('Part 1:', s)
