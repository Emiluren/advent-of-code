from pathlib import Path
import re

indata = Path('3input').read_text()

sum1 = 0
sum2 = 0
enabled = True

for m in re.finditer(r"mul\((\d+),(\d+)\)|do\(\)|don't\(\)", indata):
    match m.group():
        case "do()":
            enabled = True
        case "don't()":
            enabled = False
        case other:
            prod = int(m.group(1)) * int(m.group(2))
            sum1 += prod
            if enabled:
                sum2 += prod

print('Part 1:', sum1)
print('Part 2:', sum2)

