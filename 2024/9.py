from pathlib import Path

input_data = [int(i) for i in Path('9input').read_text().strip()]

disk = []

id_n = 0
free_space = False
for n in input_data:
    item = None if free_space else id_n
    disk += [item] * n
    if not free_space:
        id_n += 1
    free_space = not free_space

free_p = disk.index(None)
end_p = len(disk) - 1

while True:
    while disk[free_p] is not None:
        free_p += 1
    while disk[end_p] is None:
        end_p -= 1

    if end_p <= free_p:
        break

    disk[free_p] = disk[end_p]
    disk[end_p] = None

checksum = sum(i*n for i, n in enumerate(disk) if n is not None)
print('Part 1:', checksum)
