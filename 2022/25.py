def from_snafu(s):
    n = 0
    e = 1
    for c in reversed(s):
        if c == '-':
            i = -1
        elif c == '=':
            i = -2
        else:
            i = int(c)

        n += i * e
        e *= 5
    return n

def to_snafu(n):
    s = ''
    digits = [0 for i in range(32)]
    for i in range(32):
        digits[i] += (n % 5)
        n //= 5

        if digits[i] > 2:
            digits[i+1] += 1
            digits[i] -= 5
        elif digits[i] < -2:
            digits[i+1] -= 1
            digits[i] += 5

        if digits[i] >= 0:
            s += str(digits[i])
        else:
            s += ['=', '-'][digits[i]]

    return ''.join(list(reversed(s))).lstrip('0')

snafu_sum = 0
with open('25input') as f:
    for l in f:
        if len(l) > 0:
            snafu_sum += from_snafu(l.strip())

print('Part 1:', to_snafu(snafu_sum))
