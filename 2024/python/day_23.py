#!/usr/bin/env python3

from pathlib import Path
from collections import defaultdict

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input_data: str):
    links = defaultdict[str, set[str]](set)
    for left, right in map(lambda x: x.split("-"), input_data.splitlines()):
        links[left].add(right)
        links[right].add(left)
    triplets = set[tuple[str, str, str]]()
    for fst_computer in links.keys():
        for snd_computer in links[fst_computer]:
            for trd_computer in links[snd_computer]:
                if trd_computer in links[fst_computer]:
                    triplets.add(tuple(sorted([fst_computer, snd_computer, trd_computer])))
    filtered_triplets = [triplet for triplet in triplets
                         if any(map(lambda x: x.startswith("t"), triplet))]
    print("Part 1:", len(filtered_triplets))


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
