with open('21input') as f:
    world = [list('#' + line.strip() + '#') for line in f]

wall = [list('#' * len(world[0]))]
world = wall + world + wall

for r, line in enumerate(world):
    try:
        c = line.index('S')
        s = r, c
        break
    except ValueError:
        pass

reachable = set()
explored = set()

world[s[0]][s[1]] = '.'

def explore(r, c, steps):
    if steps == 64:
        reachable.add((r, c))
        return

    def check(r, c):
        if (r, c, steps+1) not in explored and world[r][c] == '.':
            explored.add((r, c, steps+1))
            return True
        return False

    if check(r-1, c):
        explore(r-1, c, steps+1)
    if check(r+1, c):
        explore(r+1, c, steps+1)
    if check(r, c-1):
        explore(r, c-1, steps+1)
    if check(r, c+1):
        explore(r, c+1, steps+1)

explore(s[0], s[1], 0)

print('Part 1:', len(reachable)) # 3718 too low

# for r, line in enumerate(world):
#     for c, t in enumerate(line):
#         if (r, c) in reachable:
#             print('O', end='')
#         else:
#             print(t, end='')
#     print()
