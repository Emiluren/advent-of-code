with open('4input') as f:
    input = list(f)

points = 0
for line in input:
    card_str, num_str = line.split(':')
    my_str, win_str = num_str.split('|')

    intersect = set(my_str.split()).intersection(set(win_str.split()))
    matches = len(intersect)

    if matches > 0:
        points += 1 << matches - 1

print('Part 1:', points)