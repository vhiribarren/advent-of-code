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
                    
                

def part_2(input: str):
    ...


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)