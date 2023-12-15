from functools import reduce

with open('15input') as f:
    data = f.read().strip().split(',')

print('Part 1:', sum(reduce(lambda acc, c: (acc + ord(c))*17 % 256, step, 0) for step in data))

