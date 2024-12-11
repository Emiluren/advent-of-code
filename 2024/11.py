from pathlib import Path

input_str = Path('11input').read_text()

numbers = input_str.split()

def iterate(ns):
    new_ns = []
    for n in ns:
        if n == '0':
            new_ns.append('1')
        elif len(n) % 2 == 0:
            new_ns.append(n[:len(n)//2])
            new_ns.append(str(int(n[len(n)//2:])))
        else:
            new_ns.append(str(int(n)*2024))
    return new_ns

for i in range(25):
    numbers = iterate(numbers)

print('Part 1:', len(numbers))

# Freezes computer
# for i in range(50):
#     numbers = iterate(numbers)

# print('Part 2:', len(numbers))
