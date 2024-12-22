from pathlib import Path

nums = [int(l) for l in Path('22input').read_text().splitlines() if l]

def iterate(n):
    n = ((n * 64) ^ n) % 16777216
    n = ((n // 32) ^ n) % 16777216
    return ((n * 2048) ^ n) % 16777216

res = 0
for n in nums:
    for i in range(2000):
        n = iterate(n)
    res += n

print('Part 1:', res)
