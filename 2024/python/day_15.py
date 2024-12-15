#!/usr/bin/env python3

from pathlib import Path
from enum import Enum, auto

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

class ElemType(Enum):
    Box = auto()
    Wall = auto()


def part_1(input_data: str):
    grid_raw, inst_raw = input_data.split("\n\n")
    instructions = []
    for c in inst_raw.strip():
        match c:
            case ">": instructions.append(1)
            case "<": instructions.append(-1)
            case "^": instructions.append(-1j)
            case "v": instructions.append(1j)
    grid = {}
    robot = None
    for y, line in enumerate(grid_raw.splitlines()):
        for x, c in enumerate(line):
            match c:
                case "#": grid[complex(x, y)] = ElemType.Wall
                case "O": grid[complex(x, y)] = ElemType.Box
                case "@": robot = complex(x, y)
    for inst in instructions:
        scan_pos = robot
        scanned_elems = []
        while True:
            scan_pos += inst
            match grid.get(scan_pos):
                case ElemType.Wall:
                    break
                case ElemType.Box:
                    scanned_elems.append(scan_pos)
                case None:
                    robot += inst
                    for elem in reversed(scanned_elems):
                        del grid[elem]
                        grid[elem + inst] = ElemType.Box
                    break
    gps_list = []
    for coord, elem in grid.items():
        if elem == ElemType.Box:
            gps_list.append(coord.real + 100*coord.imag)
    print("Part 1:", int(sum(gps_list)))



def part_2(input_data: str):
    ...


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
