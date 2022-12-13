input = []

with open("13input") as f:
    while line1 := f.readline():
        line2 = f.readline()
        f.readline()

        input.append((eval(line1), eval(line2)))

def is_valid_packet(left, right):
    #print("Compare", p1, "vs", p2)
    while True:
        if len(left) == 0:
            return True
        if len(right) == 0:
            return False

        l0 = left[0]
        r0 = right[0]

        if isinstance(l0, int):
            if isinstance(r0, int):
                if l0 < r0:
                    return True
                elif l0 > r0:
                    return False
            else:
                return is_valid_packet([l0], r0)
        else:
            if isinstance(r0, int):
                return is_valid_packet(l0, [r0])
            elif not is_valid_packet(l0, r0):
                return False

        left = left[1:]
        right = right[1:]

def part1():
    sum = 0
    i = 1
    for (p1, p2) in input:
        #print("i =", i)
        if is_valid_packet(p1, p2):
            sum += i
        i += 1
        #print()

    return sum
