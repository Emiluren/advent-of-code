from pathlib import Path

lines = Path('4testinput').read_text().split('\n')
transposed = [''.join(s) for s in zip(*lines)]

xmas_count = sum(l.count('XMAS') for l in lines)
xmas_count += sum(l[::-1].count('XMAS') for l in lines)
xmas_count += sum(l.count('XMAS') for l in transposed)
xmas_count += sum(l[::-1].count('XMAS') for l in transposed)

diagonals = []
for r in len(lines):
    pass

print('Part 1:', xmas_count)
