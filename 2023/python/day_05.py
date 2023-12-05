#!/usr/bin/env python3

import os
from dataclasses import dataclass

INPUT_FILE = "day_05.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


@dataclass
class ConversionElement:
    start: int
    end: int
    target: int

type ConversionMap = list[ConversionElement]
type Seeds = list[int]
type Maps = list[ConversionMap]

def parse_input(input: str) -> (Seeds, Maps):
    lines = input.splitlines()
    seeds = [int(s) for s in lines[0].split(":")[1].split()]
    maps = []
    line_index = 3
    conv_map = []
    while line_index < len(lines):
        v = [int(c) for c in lines[line_index].split()]
        conv_map.append(ConversionElement(v[1], v[1]+v[2]-1, v[0]))
        if line_index + 1 >= len(lines) or not lines[line_index+1]:
            line_index += 3
            maps.append(conv_map)
            conv_map = []
        else:
            line_index += 1
    return (seeds, maps)

def part_1(input: str):
    seeds, maps = parse_input(input)
    soils = []
    for seed in seeds:
        current_layer = seed
        for m in maps:
            for conv_elem in m:
                if conv_elem.start <= current_layer <= conv_elem.end:
                    current_layer = conv_elem.target + current_layer - conv_elem.start
                    break
        soils.append(current_layer)
    print("Result Part 1:", min(soils))
                    
def split_range(check_range: tuple[int, int], ref_range: tuple[int, int]):
    intersect = (max(check_range[0], ref_range[0]), min(check_range[1], ref_range[1]))
    if intersect[1] < intersect[0]:
        return (None, [check_range])
    rest = []
    before = (check_range[0], min(check_range[1], intersect[0]-1))
    if before[0] <= before[1]:
        rest.append(before)
    after = (max(intersect[1]+1, check_range[0]), check_range[1])
    if after[0] <= after[1]:
        rest.append(after)
    return (intersect, rest)

def part_2(input: str):
    # Definitively not performant enough
    # even if it took "only" 15 min on my computer on one thread

    seed_info, layers = parse_input(input)
    seed_ranges = []
    for seed_ranges_idx in range(0, len(seed_info), 2 ):
        seed_start = seed_info[seed_ranges_idx]
        seed_end = seed_start + seed_info[seed_ranges_idx+1]
        seed_ranges.append((seed_start, seed_end))

    current_ranges = seed_ranges
    next_layer_ranges = []
    for layer in layers:
        print("next")
        for current_range in current_ranges:
            intersect_found = False
            for conv_elem in layer:
                (intersect, rest) = split_range(current_range, (conv_elem.start, conv_elem.end))
                if intersect is not None:
                    intersect_found = True
                    current_ranges.extend(rest)
                    delta = conv_elem.target - conv_elem.start
                    next_layer_ranges.append((intersect[0]+ delta, intersect[1]+ delta))
            if not intersect_found:
                next_layer_ranges.append(current_range)

        current_ranges = next_layer_ranges
        next_layer_ranges = []
                    
    print("Result Part 2:", min([r[0] for r in current_ranges]))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)