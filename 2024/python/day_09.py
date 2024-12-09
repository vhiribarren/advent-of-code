#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(diskmap: str):
    disk = []
    for i, c in enumerate(diskmap):
        disk.extend([i//2 if i%2 == 0 else None for _ in range(0, int(c))])     
    idx_left, idx_right = 0, len(disk)-1
    while True:
        while disk[idx_left] != None and idx_left <= idx_right:
            idx_left += 1
        while disk[idx_right] == None and idx_right >= idx_left:
            idx_right -= 1
        if idx_left > idx_right:
            break
        disk[idx_left] = disk[idx_right]
        disk[idx_right] = None
    print("Part 1:", sum([i*v for i, v in enumerate(disk[:idx_left])]))


def part_2(diskmap: str):
    empty_slot_len = list(map(int, diskmap[1::2]))
    empty_slot_data = [[] for _ in empty_slot_len]
    orig_data_count = list(map(int, diskmap[::2]))
    # Moving data to empty slots
    for idx_right in range(len(orig_data_count)-1, -1, -1):
        for idx_left in range(0, idx_right):
            movable_data_count = orig_data_count[idx_right]
            if empty_slot_len[idx_left] >= movable_data_count:
                empty_slot_data[idx_left].extend([idx_right]*movable_data_count)
                empty_slot_len[idx_left] -= movable_data_count
                orig_data_count[idx_right] = None
                break
    # Recreating the disk content using the previous information
    disk = []
    for idx in range(0, len(diskmap)):
        block_id = idx//2
        if idx % 2 == 0:
            if orig_data_count[block_id] is not None:
                disk.extend([block_id]*orig_data_count[block_id])
            else:
                disk.extend([None]*int(diskmap[idx]))
        else:
            disk.extend(empty_slot_data[block_id])
            disk.extend([None]* empty_slot_len[block_id] )
    print("Part 2:", sum([i*v for i, v in enumerate(disk) if v is not None]))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)