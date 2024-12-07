from pathlib import Path

lines = Path('7input').read_text().strip().split('\n')

def can_be_solved(test_value, vals):
    positions = len(vals) - 1
    for i in range(2 ** positions):
        res = vals[0]
        for j in range(positions):
            if (1 << j) & i > 0:
                res += vals[j + 1]
            else:
                res *= vals[j + 1]
        if res == test_value:
            return True

def can_be_solved2(test_value, vals):
    positions = len(vals) - 1
    for i in range(3 ** positions):
        res = vals[0]
        for j in range(positions):
            asdf = (i // (3 ** j)) % 3
            if asdf == 0:
                res += vals[j + 1]
            elif asdf == 1:
                res *= vals[j + 1]
            else:
                res = int(str(res) + str(vals[j + 1]))
        if res == test_value:
            return True

total_res = 0
total_res2 = 0

for l in lines:
    test_value_str, vals_str = l.split(': ')
    test_value = int(test_value_str)
    vals = [int(v) for v in vals_str.split()]

    if can_be_solved(test_value, vals):
        total_res += test_value
    if can_be_solved2(test_value, vals):
        total_res2 += test_value

print('Part 1:', total_res)
print('Part 2:', total_res2)

