from pathlib import Path

nums = [int(l) for l in Path('22input').read_text().splitlines() if l]

def iterate(n):
    n = ((n * 64) ^ n) % 16777216
    n = ((n // 32) ^ n) % 16777216
    return ((n * 2048) ^ n) % 16777216

res = 0
changes = set()
for n in nums:
    diffs = []
    for i in range(2000):
        n = iterate(n)
        if i > 0:
            diffs.append((n % 10) - last_n)
        last_n = n % 10
    res += n

    for i in range(2000-4):
        changes.add(tuple(diffs[i:i+4]))

print('Part 1:', res)
