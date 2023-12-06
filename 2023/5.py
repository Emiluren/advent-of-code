with open('5_test_input') as f:
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


# Original structure gives me a headache, change it to (start, end, offset)
maps_alt = [[(src, src+l, dst-src) for dst, src, l in m] for m in maps]


# Just to test that maps_alt is correct
# Part 1: min(lfs_alt(s) for s in seeds)
def lfs_alt(s):
    for m in maps_alt:
        for start, end, offset in m:
            if s >= start and s < end:
                s += offset
                break
    return s


# TODO: mistake in this, should combine maps instead of producing new seed ranges
# need connection between seed (first stage) and location (last)

def map_range(start, end, m):
    i = start
    ranges = []
    for m_start, m_end, offset in m:
        if i >= end:
            break

        if i < m_start:
            ranges.append((i, m_start - i))
            i = m_start

        if m_end <= end:
            ranges.append((i+offset, m_end+offset))
            i = m_end
        else:
            ranges.append((i+offset, end+offset))
            i = end
            break

    if i < end:
        ranges.append((i, end))

    return ranges



seed_ranges = [(seeds[i], seeds[i] + seeds[i+1]) for i in range(0, len(seeds), 2)]


srs = seed_ranges
for m in maps_alt[:1]:
    new_srs = []
    for start, end in srs:
        new_srs += map_range(start, end, m)
    srs = new_srs
