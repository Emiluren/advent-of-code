with open('10input') as f:
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

while True:
    new_pos = add_pos(pos, d)
    if new_pos == start_pos:
        break

    last_pos = pos
    pos = new_pos
    row, col = pos
    tile = world[row][col]

    positions.add(pos)
    for diff in conns[tile]:
        if add_pos(pos, diff) != last_pos:
            d = diff
            break

print('Part 1:', loop_length//2 + 1)
