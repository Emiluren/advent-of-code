with open('21input') as f:
    world = [list(line.strip()) for line in f]

for r, line in enumerate(world):
    try:
        c = line.index('S')
        s = r, c
        break
    except ValueError:
        pass

reachable1 = set()
explored1 = set()

world[s[0]][s[1]] = '.'

def explore1(r, c, steps):
    if (r, c, steps) in explored1 or \
       r < 0 or r >= len(world) or \
       c < 0 or c >= len(world[0]) or \
       world[r][c] == '#':
        return
    explored1.add((r, c, steps))

    if steps == 64:
        reachable1.add((r, c))
        return

    explore1(r-1, c, steps+1)
    explore1(r+1, c, steps+1)
    explore1(r, c-1, steps+1)
    explore1(r, c+1, steps+1)

explore1(s[0], s[1], 0)

print('Part 1:', len(reachable1)) # 3718 too low

reachable2 = set()
explored2 = set()

def explore2(r, c, steps):
    if (r, c, steps) in explored2 or \
       world[r % len(world)][c % len(world[0])] == '#':
        return
    explored2.add((r, c, steps))

    if steps == 26501365:
        reachable2.add((r, c))
        return

    explore2(r-1, c, steps+1)
    explore2(r+1, c, steps+1)
    explore2(r, c-1, steps+1)
    explore2(r, c+1, steps+1)

# import sys
# sys.setrecursionlimit(100000000)
# explore2(s[0], s[1], 0)

# print('Part 2:', len(reachable2))

# for r, line in enumerate(world):
#     for c, t in enumerate(line):
#         if (r, c) in reachable:
#             print('O', end='')
#         else:
#             print(t, end='')
#     print()
