#!/usr/bin/env python3

import os.path as op
from enum import Enum, auto
from dataclasses import dataclass

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


class Action(Enum):
    Toggle = auto()
    On = auto()
    Off = auto()

@dataclass
class Instruction:
    action: Action
    x_start: int
    y_start: int
    x_end: int
    y_end: int

def parse_instructions(input: str) -> list[Instruction]:
    result = []
    for line in input.splitlines():
        match l := line.split():
            case ["toggle", *_]:
                result.append(Instruction(Action.Toggle, *map(int, l[1].split(",")), *map(int, l[3].split(","))))
            case [_, "on", *_]:
                result.append(Instruction(Action.On, *map(int, l[2].split(",")), *map(int, l[4].split(","))))
            case [_, "off", *_]:
                result.append(Instruction(Action.Off, *map(int, l[2].split(",")), *map(int, l[4].split(","))))
    return result

def part_1(input: str):
    width, height = 1000, 1000
    grid = [0] * width * height
    for inst in parse_instructions(input):
        for x in range(inst.x_start, inst.x_end+1):
            for y in range(inst.y_start, inst.y_end+1):
                coord = x + y * width
                match inst.action:
                    case Action.Off: grid[coord] = 0
                    case Action.On: grid[coord] = 1
                    case Action.Toggle: grid[coord] = 1 - grid[coord]
    print("Part 1:", sum(grid))

def part_2(input: str):
    width, height = 1000, 1000
    grid = [0] * width * height
    for inst in parse_instructions(input):
        for x in range(inst.x_start, inst.x_end+1):
            for y in range(inst.y_start, inst.y_end+1):
                coord = x + y * width
                match inst.action:
                    case Action.Off: grid[coord] = max(0, grid[coord] - 1)
                    case Action.On: grid[coord] += 1
                    case Action.Toggle: grid[coord] += 2
    print("Part 2:", sum(grid))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)