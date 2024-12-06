from pathlib import Path

lines = Path('4input').read_text().strip().split('\n')
transposed = [''.join(s) for s in zip(*lines)]

xmas_count = sum(l.count('XMAS') for l in lines)
xmas_count += sum(l[::-1].count('XMAS') for l in lines)
xmas_count += sum(l.count('XMAS') for l in transposed)
xmas_count += sum(l[::-1].count('XMAS') for l in transposed)

height = len(lines)
width = len(lines[0])

fdiags = ["" for _ in range(width + height - 1)]
bdiags = ["" for _ in range(width + height - 1)]
min_bdiag = 1 - height

for r in range(height):
    for c in range(width):
        fdiags[c+r] += lines[r][c]
        bdiags[c-r-min_bdiag] += lines[r][c]

xmas_count += sum(l.count('XMAS') for l in fdiags)
xmas_count += sum(l[::-1].count('XMAS') for l in fdiags)
xmas_count += sum(l.count('XMAS') for l in bdiags)
xmas_count += sum(l[::-1].count('XMAS') for l in bdiags)

print('Part 1:', xmas_count)

count2 = 0
for r in range(height - 2):
    for c in range(width - 2):
        d1 = lines[r][c] + lines[r+1][c+1] + lines[r+2][c+2]
        d2 = lines[r+2][c] + lines[r+1][c+1] + lines[r][c+2]

        if (d1 == 'MAS' or d1 == 'SAM') and (d2 == 'MAS' or d2 == 'SAM'):
            count2 += 1

print('Part 2:', count2)
