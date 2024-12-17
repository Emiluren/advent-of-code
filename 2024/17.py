from pathlib import Path

lines = Path('17input').read_text().strip().splitlines()

a = int(lines[0][12:])
b = int(lines[1][12:])
c = int(lines[2][12:])
program = [int(i) for i in lines[4][9:].split(',')]

pc = 0

outputs = []

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
            outputs.append(combo(program[pc+1]) % 8)
        case 6: # bdv
            b = a >> combo(program[pc+1])
        case 7: # cdv
            c = a >> combo(program[pc+1])

    if not jumps:
        pc += 2

print('Part 1:', ','.join(str(i) for i in outputs))
