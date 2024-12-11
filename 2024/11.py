from pathlib import Path

input_str = Path('11input').read_text()

numbers = input_str.split()

def subdivide(n):
    if n == '0':
        return ['1']
    elif len(n) % 2 == 0:
        return [n[:len(n)//2], str(int(n[len(n)//2:]))]
    else:
        return [str(int(n)*2024)]

mem = {}
def iterate(n, times):
    if (n, times) in mem:
        return mem[(n, times)]

    if times == 0:
        return 1

    result = sum(iterate(n2, times-1) for n2 in subdivide(n))
    mem[(n, times)] = result
    return result

print('Part 1:', sum(iterate(n, 25) for n in numbers))
print('Part 2:', sum(iterate(n, 75) for n in numbers))
