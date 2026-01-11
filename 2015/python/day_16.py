#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

TARGET = {
    "children": 3,
    "cats": 7,
    "samoyeds": 2,
    "pomeranians": 3,
    "akitas": 0,
    "vizslas": 0,
    "goldfish": 5,
    "trees": 3,
    "cars": 2,
    "perfumes": 1,
}

def parse(input_data: str):
    sues = []
    for line in input_data.splitlines():
        elems_dict = {}
        for elems in line[line.find(":")+1:].split(","):
            k, v = elems.split(":")
            elems_dict[k.strip()] = int(v)
        sues.append(elems_dict)
    return sues

def part_1(input_data: str):
    sues = parse(input_data)
    for (idx, sue) in enumerate(sues):
        for tkey, tval in TARGET.items():
            if tkey in sue and sue[tkey] != tval:
                break
        else:
            print("Part 1:", idx + 1)

def part_2(input_data: str):
    sues = parse(input_data)
    for (idx, sue) in enumerate(sues):
        for tkey, tval in TARGET.items():
            if tkey not in sue:
                continue
            match tkey:
                case "cats" | "trees":
                    if not sue[tkey] > tval:
                        break
                case "pomeranians" | "goldfish":
                    if not sue[tkey] < tval:
                        break
                case _:
                    if not sue[tkey] == tval:
                        break
        else:
            print("Part 1:", idx + 1)

if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
