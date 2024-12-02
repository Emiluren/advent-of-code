from pathlib import Path

lines = Path('2input').read_text().split('\n')

parsed_lines = [[int(i) for i in l.split(' ')] for l in lines if l]

safe_count = 0
for l in parsed_lines:
    ds = [a-b for a,b in zip(l[1:], l[:-1])]
    if (all(d < 0 for d in ds) or all(d > 0 for d in ds)) and all(abs(d) <= 3 for d in ds):
        safe_count += 1

print('Part 1:', safe_count)
