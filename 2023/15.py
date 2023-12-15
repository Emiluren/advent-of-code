from functools import reduce

with open('15input') as f:
    data = f.read().strip().split(',')

def hash(s):
    return reduce(lambda acc, c: (acc + ord(c))*17 % 256, s, 0)

print('Part 1:', sum(hash(step) for step in data))

boxes: list[list[tuple[str, int]]] = [[] for i in range(256)]
for step in data:
    if '-' in step:
        label = step.rstrip('-')
        box = boxes[hash(label)]
        try:
            i = next(i for i, (lab2, lens2) in enumerate(box) if lab2 == label)
            del box[i]
        except StopIteration:
            pass
    else:
        label, lens = step.split('=')
        box = boxes[hash(label)]

        try:
            i = next(i for i, (lab2, lens2) in enumerate(box) if lab2 == label)
            box[i] = (label, int(lens))
        except StopIteration:
            box.append((label, int(lens)))

print('Part 2:', sum(
    sum((i+1)*(boxi+1)*lens for i, (label, lens) in enumerate(box))
    for boxi, box in enumerate(boxes)
))
