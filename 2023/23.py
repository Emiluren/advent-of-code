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
