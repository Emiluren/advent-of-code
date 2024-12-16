from pathlib import Path

lines = Path('12input').read_text().strip().split('\n')

height = len(lines)
width = len(lines[0])

def island_from(r, c):
    island = set()

    def search(r, c):
        if (r, c) in island:
            return
        island.add((r, c))

        if r > 0 and lines[r-1][c] == lines[r][c]:
            search(r-1, c)
        if c > 0 and lines[r][c-1] == lines[r][c]:
            search(r, c-1)
        if r < height-1 and lines[r+1][c] == lines[r][c]:
            search(r+1, c)
        if c < width-1 and lines[r][c+1] == lines[r][c]:
            search(r, c+1)

    search(r, c)
    return frozenset(island)

all_coords = [(r, c) for c in range(width) for r in range(height)]
all_islands = {island_from(r, c) for r, c in all_coords}

def area(c):
    return sum(l.count(c) for l in lines)

def count_edges(ls, c):
    edge_count = 0

    for i, l in enumerate(ls):
        for col in range(len(l)):
            if l[col] != c:
                continue

            if i == 0 or ls[i-1][col] != c:
                edge_count += 1
            if i == len(lines)-1 or ls[i+1][col] != c:
                edge_count += 1

    return edge_count

def perimeter(island):
    count = 0
    dirs = [(-1, 0), (1, 0), (0, -1), (0, 1)]
    for (r, c) in island:
        for dr, dc in dirs:
            if (r+dr, c+dc) not in island:
                count += 1
    return count

total = sum(len(i) * perimeter(i) for i in all_islands)

print('Part 1:', total)
