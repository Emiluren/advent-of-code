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

disk2 = disk[:]

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

def checksum(d):
    return sum(i*n for i, n in enumerate(d) if n is not None)

print('Part 1:', checksum(disk))

end_p = len(disk2) - 1
while end_p > 0 and disk2[end_p] is None:
    end_p -= 1

max_id = disk2[end_p]

for current in range(max_id, -1, -1):
    while disk2[end_p] != current:
        end_p -= 1

    block_end = end_p
    while disk2[end_p - 1] == disk2[end_p]:
        end_p -= 1

    block_size = block_end - end_p + 1

    free_p = 0
    while free_p < end_p:
        while disk2[free_p] is not None:
            free_p += 1

        free_size = 1
        while disk2[free_p + free_size] is None:
            free_size += 1

        if free_size >= block_size:
            break
        while disk2[free_p] is None:
            free_p += 1

    if free_p >= end_p:
        continue

    for i in range(block_size):
        disk2[free_p + i] = disk2[end_p + i]
        disk2[end_p + i] = None

print('Part 2:', checksum(disk2))
