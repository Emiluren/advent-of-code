with open('4input') as f:
    input = list(f)

points = 0
copies = [1 for i in range(len(input))]
for i, line in enumerate(input):
    card_str, num_str = line.split(':')
    my_str, win_str = num_str.split('|')

    intersect = set(my_str.split()).intersection(set(win_str.split()))
    matches = len(intersect)

    if matches > 0:
        points += 1 << matches - 1
        for j in range(matches):
            copies[i+j+1] += copies[i]

print('Part 1:', points)
print('Part 2:', sum(copies))
