with open('14input') as f:
    rows = [line for line in f]

cols = list(list(c) for c in zip(*rows))

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

print('Part 1:', sum(sum(len(c) - i for i, t in enumerate(c) if t == 'O') for c in cols))
