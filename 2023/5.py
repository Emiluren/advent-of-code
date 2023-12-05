with open('5input') as f:
    input = f.read(None)

split_input = input.split('\n\n')
seeds = [int(s) for s in split_input[0].split()[1:]]
str_maps = [m.strip().split('\n')[1:] for m in split_input[1:]]

maps = [sorted([[int(v) for v in s.split()] for s in sm], key=lambda entry: entry[1]) for sm in str_maps]

def location_for_seed(s):
    for m in maps:
        for dst, src, l in m:
            if s >= src and s < src + l:
                s = s - src + dst
                break
    return s

print('Part 1:', min(location_for_seed(s) for s in seeds))
