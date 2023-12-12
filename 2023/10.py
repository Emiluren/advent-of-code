with open('10_test_input') as f:
    world = [line for line in f]

conns = {
    '|': [(-1, 0), (1, 0)],
    '-': [(0, -1), (0, 1)],
    'L': [(-1, 0), (0, 1)],
    'J': [(-1, 0), (0, -1)],
    '7': [(0, -1), (1, 0)],
    'F': [(1, 0), (0, 1)],
    '.': []
}

for (row, line) in enumerate(world):
    col = line.find('S')
    if col != -1:
        start_row, start_col = start_pos = (row, col)

def add_pos(p, diff):
    r, c = p
    dr, dc = diff
    return r+dr, c+dc

start_dirs = []

for dr in [-1, 0, 1]:
    for dc in [-1, 0, 1]:
        if dr == 0 and dc == 0:
            continue

        r = start_row+dr
        c = start_col+dc
        tile = world[r][c]

        for dr2, dc2 in conns[tile]:
            if dr+dr2 == 0 and dc+dc2 == 0:
                start_dirs.append((dr, dc))


sd = start_dirs[0]

pos = start_pos
d = sd

positions = set(start_pos)

top = bottom = start_pos[0]
left = right = start_pos[1]

while True:
    new_pos = add_pos(pos, d)
    if new_pos == start_pos:
        break

    last_pos = pos
    pos = new_pos
    row, col = pos
    tile = world[row][col]

    top = min(row, top)
    bottom = max(row, bottom)
    left = min(col, left)
    right = max(col, right)

    positions.add(pos)
    for diff in conns[tile]:
        if add_pos(pos, diff) != last_pos:
            d = diff
            break

print('Part 1:', loop_length//2 + 1)

area = 0

for r in range(top, bottom+1):
    # TODO: not enough states F-7 gives wrong result for example
    inside = False
    for c in range(left, right+1):
        if (r, c) in positions:
            inside = not inside
            print(world[r][c], end='')
        else:
            if inside:
                area += 1
                print('I', end='')
            else:
                print('O', end='')
    print()

# 2189 too high
# 1813
print('Part 2:', area)
