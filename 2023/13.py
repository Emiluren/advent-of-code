with open('13input') as f:
    patterns = [[list(l) for l in p.split('\n')] for p in f.read().strip().split('\n\n')]

def find_mirror_point(rows):
    height = len(rows)
    for i in range(1, height):
        slice_height = min(i, height-i)
        top = rows[i-slice_height:i]
        bottom = list(reversed(rows[i:i+slice_height]))
        if top == bottom:
            yield i

def print_pattern(p):
    print('rows')
    rows = map(''.join, p)
    cols = map(''.join, zip(*p))
    for r in rows:
        print(r)
    print()
    print('cols', cols)
    for c in cols:
        print(c)
    print()


def find_sol(p):
    mp = [100*mp for mp in find_mirror_point(p)]

    cols = list(zip(*p))
    return mp + list(find_mirror_point(cols))


solutions = [find_sol(p)[0] for p in patterns]
print('Part 1:', sum(solutions))

s2 = 0
for p, old in zip(patterns, solutions):
    try:
        for r in range(len(p)):
            for c in range(len(p[0])):
                if p[r][c] == '.':
                    p[r][c] = '#'
                    sol2 = find_sol(p)
                    p[r][c] = '.'
                else:
                    p[r][c] = '.'
                    sol2 = find_sol(p)
                    p[r][c] = '#'

                for s in sol2:
                    if s != old:
                        s2 += s
                        raise StopIteration
    except StopIteration:
        pass

print('Part 2:', s2) # 22170 too low, 36841 too high
