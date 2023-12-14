with open('14input') as f:
    rows = [line.strip() for line in f]

cols = list(list(c) for c in zip(*rows))


def stringify(cols):
    return '\n'.join(''.join(t for t in c) for c in cols)


def tilt(cols):
    from copy import deepcopy
    cols = deepcopy(cols)
    for col in cols:
        while True:
            changed = False

            for i in range(len(col) - 1):
                if col[i] == '.' and col[i+1] == 'O':
                    col[i] = 'O'
                    col[i+1] = '.'
                    changed = True

            if not changed:
                break
    return cols


def calc_load(cols):
    return sum(sum(len(c) - i for i, t in enumerate(c) if t == 'O') for c in cols)


def rotate(rows):
    l = list(list(c) for c in zip(*rows))
    l.reverse()
    return l


print('Part 1:', calc_load(tilt(cols)))


states = [stringify(cols)]
cycled = cols
while True:
    for i in range(4):
        cycled = rotate(tilt(cycled))

    cs = stringify(cycled)
    try:
        ind = states.index(cs)
        repeat_length = len(states) - ind
        print('Part 2:', calc_load(states[ind + (1000000000 - ind) % repeat_length].splitlines()))
        break
    except ValueError:
        states.append(cs)
        cols = cycled

# 99430 too low
