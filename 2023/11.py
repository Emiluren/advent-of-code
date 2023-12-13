with open('11input') as f:
    world = [line for line in f]

width, height = len(world[0]), len(world)

galaxies = []

empty_rows = []

for (row, line) in enumerate(world):
    row_empty = True
    for (col, c) in enumerate(line):
        if c == '#':
            galaxies.append((row, col))
            row_empty = False
    if row_empty:
        empty_rows.append(row)

empty_cols = [col for col in range(width) if all(world[row][col] == '.' for row in range(height))]


expanded_galaxies = [(
    r + len([er for er in empty_rows if er < r]),
    c + len([ec for ec in empty_cols if ec < c])
) for r, c in galaxies]

# for r in range(height*2):
#     for c in range(width*2):
#         if (r, c) in expanded_galaxies:
#             print('#', end='')
#         else:
#             print('.', end='')
#     print()

from itertools import combinations

dist = 0
for ((r1, c1), (r2, c2)) in combinations(expanded_galaxies, 2):
    d = r2-r1 + abs(c2-c1)
    dist += d
    #print(f'Between {i+1} and {i+1+j+1}: {d}')

print('Part 1:', dist)


expanded_galaxies2 = [(
    r + 999999*len([er for er in empty_rows if er < r]),
    c + 999999*len([ec for ec in empty_cols if ec < c])
) for r, c in galaxies]

from itertools import combinations

dist2 = 0
for ((r1, c1), (r2, c2)) in combinations(expanded_galaxies2, 2):
    d = r2-r1 + abs(c2-c1)
    dist2 += d

print('Part 2:', dist2)
