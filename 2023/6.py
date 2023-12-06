from functools import reduce
from operator import mul

def part1(times, distances):
    return reduce(mul, (race_possibilities(t, d) for t, d in zip(times, distances)))

def race_possibilities(time, dist):
    poss = 0
    for t in range(1, time):
        if t * (time - t) > dist:
            poss += 1
    return poss
