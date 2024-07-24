#!/usr/bin/env python3

import os
from enum import Enum
from dataclasses import dataclass

INPUT_FILE = "day_14.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


class CellValue(Enum):
    Rock = 1
    Obstacle = 2

@dataclass
class Field:
    width: int
    height: int
    elements: dict[complex, CellValue]


def part_1(input: str):
    field = generate_field(input)
    elems = field.elements
    width = field.width
    height = field.height
    for y in range(1, height):
        for x in range(0, width):
            if elems.get(complex(x, y)) != CellValue.Rock:
                continue
            for scan_y in range(y-1, -1, -1):
                if elems.get(complex(x, scan_y)) is not None:
                    break
            else:
                scan_y = -1
            del elems[complex(x, y)]
            elems[complex(x, scan_y +1)] = CellValue.Rock
    print("Result 1:", compute_score(field))


def part_2(input: str):
    ...

def generate_field(input: str) -> Field:
    elements = {}
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            coord = complex(x, y)
            match c:
                case "O":
                    elements[coord] = CellValue.Rock
                case "#":
                    elements[coord] = CellValue.Obstacle
    return Field(x+1, y+1, elements)

def compute_score(field: Field):
    width = field.width
    height = field.height
    elems = field.elements
    score = 0
    for x in range(0, width):
        for y in range(0, height):
            if elems.get(complex(x, y)) == CellValue.Rock:
                score += height - y
    return score

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)