with open('11_test_input') as f:
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
    r + 2*len([er for er in empty_rows if er < r]),
    c + 2*len([ec for ec in empty_cols if ec < c])
) for r, c in galaxies]


print('Part 1:', sum(r2-r1 + abs(c2-c1) for (i, (r1, c1)) in enumerate(galaxies[:-1]) for r2, c2 in galaxies[i+1:] ))
