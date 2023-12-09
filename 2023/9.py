with open('9input') as f:
    input = [[int(n) for n in line.split()] for line in f]


def predict_next(l):
    if all(n == 0 for n in l):
        return 0

    l2 = [x - y for x, y in zip(l[1:], l[:-1])]
    return l[-1] + predict_next(l2)

print('Part 1:', sum(predict_next(l) for l in input))


def predict_previous(l):
    if all(n == 0 for n in l):
        return 0

    l2 = [x - y for x, y in zip(l[1:], l[:-1])]
    return l[0] - predict_previous(l2)

print('Part 1:', sum(predict_previous(l) for l in input))
