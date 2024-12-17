from pathlib import Path

lines = Path('17input').read_text().strip().splitlines()

a = int(lines[0][12:])
program = [int(i) for i in lines[4][9:].split(',')]

def run_program(initial_a):
    outputs = []
    a = initial_a
    b = 0
    c = 0

    pc = 0

    def combo(op):
        match op:
            case op if op <= 3:
                return op
            case 4:
                return a
            case 5:
                return b
            case 6:
                return c
            case _:
                raise Exception(f"Invalid operand {op}")


    while pc < len(program):
        jumps = False
        match program[pc]:
            case 0: # adv
                a >>= combo(program[pc+1])
            case 1: # bxl
                b ^= program[pc+1]
            case 2: # bst
                b = combo(program[pc+1]) % 8
            case 3: # jnz
                if a != 0:
                    jumps = True
                    pc = program[pc+1]
            case 4: # bxc
                b ^= c
            case 5: # out
                yield combo(program[pc+1]) % 8
            case 6: # bdv
                b = a >> combo(program[pc+1])
            case 7: # cdv
                c = a >> combo(program[pc+1])

        if not jumps:
            pc += 2

print('Part 1:', ','.join(str(i) for i in run_program(a)))

# Naive part 2
# test_a = 0
# while True:
#     print('testing', test_a)
#     out = run_program(test_a)
#     if out == program:
#         print('Part 2:', test_a)
#         break
#     test_a += 1

# RE
# 2,4 bst a
# 1,5 bxl 5
# 7,5 cdv b
# 1,6 bxl 6
# 4,1 bxc 1
# 5,5 out b
# 0,3 adv 3
# 3,0 jnz 0

# while b != 0:
#     b = a % 8
#     b ^= 5
#     c = a >> b
#     b ^= 6
#     b ^= c
#     print(b % 8)
#     a >>= 3

# while a != 0:
#     b = (a >> (5 ^ (a % 8))) ^ (a % 8) ^ 3
#     print(b % 8)
#     a >>= 3

def solve(last_a, prog):
    if not prog:
        yield last_a
    else:
        for i in range(8):
            a = (last_a << 3) | i
            if next(run_program(a)) == prog[-1]:
                yield from solve(a, prog[:-1])

solutions = list(solve(0, program))
print('Part 2:', min(solutions))
