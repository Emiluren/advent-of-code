with open('23input') as f:
    world = [line.strip() for line in f]

start_col = world[0].index('.')
end_col = world[-1].index('.')


import sys
sys.setrecursionlimit(10000)

def longest_path(pos, came_from):
    if pos[0] == len(world) - 1:
        return 1

    r, c = pos
    paths = []
    if came_from != (r, c+1) and (world[r][c+1] == '.' or world[r][c+1] == '>'):
        paths.append(longest_path((r, c+1), pos))
    if came_from != (r, c-1) and (world[r][c-1] == '.' or world[r][c-1] == '<'):
        paths.append(longest_path((r, c-1), pos))
    if came_from != (r+1, c) and (world[r+1][c] == '.' or world[r+1][c] == 'v'):
        paths.append(longest_path((r+1, c), pos))
    if came_from != (r-1, c) and (world[r-1][c] == '.' or world[r-1][c] == '^'):
        paths.append(longest_path((r-1, c), pos))

    if not paths:
        return 0
    if len(paths) == 1:
        return 1 + paths[0]
    return 1 + max(*paths)


print('Part 1:', longest_path((1, start_col), (0, start_col)))


nodes = {}
end_pos = (len(world)-1, end_col)


def build_graph(pos, came_from, last_node, steps):
    if pos[0] == len(world) - 1:
        nodes[end_pos] = {last_node: steps}
        nodes[last_node][end_pos] = steps
        return

    r, c = pos
    paths = []
    for rr, cc in [(r, c+1), (r, c-1), (r+1, c), (r-1, c)]:
        if (rr, cc) != came_from and world[rr][cc] != '#':
            paths.append((rr, cc))

    if len(paths) > 1:
        nodes[last_node][pos] = steps
        if pos in nodes:
            nodes[pos][last_node] = steps
        else:
            nodes[pos] = {last_node: steps}
            for p in paths:
                build_graph(p, pos, pos, 1)
    else:
        for p in paths:
            build_graph(p, pos, last_node, steps+1)


start_pos = (0, start_col)
nodes[start_pos] = {}
build_graph((1, start_col), start_pos, start_pos, 1)


def longest_path2(pos, visited):
    if pos == end_pos:
        return 0

    longest = None

    for n in nodes[pos]:
        if n in visited:
            continue

        d = nodes[pos][n]

        visited.add(n)
        d2 = longest_path2(n, visited)
        visited.remove(n)

        if d2 is None:
            continue

        if longest is None or (d + d2) > longest:
            longest = d + d2

    return longest


print('Part 2:', longest_path2((0, start_col), {(0, start_col)}))


letter_nodes = { n: chr(ord('0') + i if i < 10 else ord('A') + i - 10) for i, n in enumerate(nodes) }
letter_graph = { letter_nodes[n]: [letter_nodes[m] for m in nodes[n]] for n in nodes }


def print_letter_graph():
    for l1 in letter_graph:
        for l2 in letter_graph:
            if l2 in letter_graph[l1]:
                print(1, end=' ')
            else:
                print(0, end=' ')
        print()


