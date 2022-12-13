import logging

logging.basicConfig(format='%(message)s')
#logging.getLogger().setLevel(logging.DEBUG)

input = []

with open("13input") as f:
    while line1 := f.readline():
        line2 = f.readline()
        f.readline()

        from ast import literal_eval
        input.append((literal_eval(line1), literal_eval(line2)))


def fmt_packet(p):
    if isinstance(p, int):
        return str(p)
    return '[' + ','.join(map(fmt_packet, p)) + ']'


def cmp_packets(left, right, depth=0, recur_list=False):
    ind = '  ' * depth + '- '
    if not recur_list:
        logging.debug(f'{ind}Compare {fmt_packet(left)} vs {fmt_packet(right)}')

    ind = '  ' * (depth + 1) + '- '
    if left == []:
        if right == []:
            return 0
        logging.debug(ind + 'Left side ran out of items, so inputs are in the right order')
        return 1

    if isinstance(left, int):
        if isinstance(right, int):
            if left < right:
                logging.debug(ind + 'Left side is smaller, so inputs are in the right order')
            elif left > right:
                logging.debug(ind + 'Right side is smaller, so inputs are not in the right order')
            return right - left
        logging.debug(ind + f'Mixed types; convert left to {[left]} and retry comparison')
        return cmp_packets([left], right, depth+1)

    if isinstance(right, int):
        logging.debug(ind + f'Mixed types; convert right to {[right]} and retry comparison')
        return cmp_packets(left, [right], depth+1)

    if right == []:
        logging.debug(ind + 'Right side ran out of items, so inputs are not in the right order')
        return -1

    ret = cmp_packets(left[0], right[0], depth+1)
    if ret > 0:
        return 1
    if ret == 0:
        return cmp_packets(left[1:], right[1:], depth, True)
    return -1


def part1():
    sum = 0
    i = 1
    for (left, right) in input:
        logging.debug(f"== Pair {i} ==")
        if cmp_packets(left, right) > 0:
            sum += i
        i += 1
        logging.debug('')
    return sum


def part2():
    i2 = 1
    i6 = 2
    for (left, right) in input:
        if cmp_packets(left, [[2]]) > 0:
            i2 += 1
        if cmp_packets(right, [[2]]) > 0:
            i2 += 1

        if cmp_packets(left, [[6]]) > 0:
            i6 += 1
        if cmp_packets(right, [[6]]) > 0:
            i6 += 1
    return i2 * i6


if __name__ == '__main__':
    print('Part 1:', part1())
    print('Part 2:', part2())
