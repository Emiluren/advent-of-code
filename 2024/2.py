from pathlib import Path

lines = Path('2input').read_text().split('\n')

parsed_lines = [[int(i) for i in l.split(' ')] for l in lines if l]

def is_safe(l):
    ds = [a-b for a,b in zip(l[1:], l[:-1])]
    return (all(d < 0 for d in ds) or all(d > 0 for d in ds)) and all(abs(d) <= 3 for d in ds)

safe_count = 0
safe_count2 = 0
for l in parsed_lines:
    if is_safe(l):
        safe_count += 1

    for i in range(len(l)):
        if is_safe(l[:i] + l[i+1:]):
            safe_count2 += 1
            break

print('Part 1:', safe_count)
print('Part 2:', safe_count2)
