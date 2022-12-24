elves = set()

with open('23input') as f:
    for r, line in enumerate(f):
        for c, tile in enumerate(line):
            if tile == '#':
                elves.add((r, c))


def world_bounds(world):
    e = [e for e in world][0]
    min_r, max_r, min_c, max_c = e[0], e[0], e[1], e[1]

    for r, c in world:
        min_c = min(min_c, c)
        min_r = min(min_r, r)
        max_c = max(max_c, c)
        max_r = max(max_r, r)

    return min_c, max_c+1, min_r, max_r+1


def print_world(world):
    min_c, max_c, min_r, max_r = world_bounds(world)

    for r in range(min_r, max_r):
        for c in range(min_c, max_c):
            if (r, c) in world:
                print('#', end='')
            else:
                print('.', end='')
        print()


def step_world(world, it):
    def is_free(r, c):
        return (r, c) not in world

    suggestions = {}
    for r, c in world:
        tests = [
            (is_free(r-1, c) and is_free(r-1, c-1) and is_free(r-1, c+1), (r-1, c)),
            (is_free(r+1, c) and is_free(r+1, c-1) and is_free(r+1, c+1), (r+1, c)),
            (is_free(r, c-1) and is_free(r-1, c-1) and is_free(r+1, c-1), (r, c-1)),
            (is_free(r, c+1) and is_free(r-1, c+1) and is_free(r+1, c+1), (r, c+1))
        ]
        if all(t[0] for t in tests):
            suggestions[(r, c)] = [(r, c)]
            continue

        new_pos = None

        for i in range(4):
            res, pos = tests[(it + i) % 4]
            if res:
                new_pos = pos
                break

        if new_pos is None:
            new_pos = (r, c)

        suggestions.setdefault(new_pos, []).append((r, c))

    new_elves = set()
    for (pos, elves_on_pos) in suggestions.items():
        if len(elves_on_pos) > 1:
            for elf in elves_on_pos:
                new_elves.add(elf)
        else:
            new_elves.add(pos)

    return new_elves


def solve():
    world = elves
    for i in range(10):
        world = step_world(world, i)

    ground_tiles = 0
    min_c, max_c, min_r, max_r = world_bounds(world)
    for r in range(min_r, max_r):
        for c in range(min_c, max_c):
            if (r, c) not in world:
                ground_tiles += 1

    print_world(world)
    print("Part 1:", ground_tiles)

    i = 10
    while True:
        new_world = step_world(world, i)
        i += 1
        if world == new_world:
            break
        world = new_world

    print_world(world)
    print("Part 2:", i)
