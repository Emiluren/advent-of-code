import logging

logging.basicConfig(format='%(message)s')
logging.getLogger().setLevel(logging.DEBUG)

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


def is_valid_packet(left, right, depth=0, recur_list=False):
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
        return is_valid_packet([left], right, depth+1)

    if isinstance(right, int):
        logging.debug(ind + f'Mixed types; convert right to {[right]} and retry comparison')
        return is_valid_packet(left, [right], depth+1)

    if right == []:
        logging.debug(ind + 'Right side ran out of items, so inputs are not in the right order')
        return -1

    ret = is_valid_packet(left[0], right[0], depth+1)
    if ret > 0:
        return 1
    if ret == 0:
        return is_valid_packet(left[1:], right[1:], depth, True)
    return -1

def part1():
    sum = 0
    i = 1
    for (left, right) in input:
        logging.debug(f"== Pair {i} ==")
        if is_valid_packet(left, right) > 0:
            sum += i
        i += 1
        logging.debug('')

    return sum

if __name__ == '__main__':
    print(part1())
