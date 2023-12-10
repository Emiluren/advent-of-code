with open('10input') as f:
    world = [line for line in f]

for (row, line) in enumerate(world):
    col = line.find('S')
    if col != -1:
        start_position = (row, col)
