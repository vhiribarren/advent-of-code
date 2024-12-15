#!/usr/bin/env python3

from pathlib import Path
from dataclasses import dataclass

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

class Wall:
    pass

@dataclass(unsafe_hash=True) # We will not mutate the object while being in a set
class Box:
    left: complex = 0
    right: complex = 0

type GridElem = Wall | Box

class BreakOuterLoop(Exception):
    pass


def parse_instructions(inst_data: str) -> list[complex]:
    return [
        { ">": 1, "<": -1, "^": -1j, "v": 1j}[c] for c in "".join(inst_data.splitlines())
    ]

def part_1(input_data: str):
    grid_raw, inst_raw = input_data.split("\n\n")
    instructions = parse_instructions(inst_raw)
    grid = dict[complex, GridElem]()
    robot = None
    for y, line in enumerate(grid_raw.splitlines()):
        for x, c in enumerate(line):
            match c:
                case "#": grid[complex(x, y)] = Wall()
                case "O": grid[complex(x, y)] = Box()
                case "@": robot = complex(x, y)
    assert robot is not None
    for inst in instructions:
        scan_pos = robot
        scanned_coords = []
        while True:
            scan_pos += inst
            match grid.get(scan_pos):
                case Wall():
                    break
                case Box():
                    scanned_coords.append(scan_pos)
                case None:
                    robot += inst
                    for scanned_coord in reversed(scanned_coords):
                        del grid[scanned_coord]
                        grid[scanned_coord + inst] = Box()
                    break
    gps_list = []
    for coord, elem in grid.items():
        if isinstance(elem, Box):
            gps_list.append(coord.real + 100*coord.imag)
    print("Part 1:", int(sum(gps_list)))


def part_2(input_data: str):
    grid_raw, inst_raw = input_data.split("\n\n")
    instructions = parse_instructions(inst_raw)
    grid = dict[complex, GridElem]()
    robot = None
    boxes = list[Box]()
    for y, line in enumerate(grid_raw.splitlines()):
        for x, c in enumerate(line):
            match c:
                case "#":
                    grid[complex(2*x, y)] = Wall()
                    grid[complex(2*x+1, y)] = Wall()
                case "O":
                    box = Box(complex(2*x, y), complex(2*x+1, y))
                    grid[box.left] = box
                    grid[box.right] = box
                    boxes.append(box)
                case "@":
                    robot = complex(2*x, y)
    assert robot is not None
    for inst in instructions:
        positions_to_scan = {robot}
        scanned_boxes = set[Box]()
        try:
            while len(positions_to_scan) > 0:
                positions_to_scan = {s + inst for s in positions_to_scan}
                positions_to_scan_copy = positions_to_scan.copy()
                for scanned_pos in positions_to_scan_copy:
                    match grid.get(scanned_pos):
                        case Wall():
                            raise BreakOuterLoop()
                        case Box() as box:
                            scanned_boxes.add(box)
                            positions_to_scan.update([box.left, box.right])                             
                            if inst == 1:
                                positions_to_scan.remove(box.left)
                            elif inst == -1:
                                positions_to_scan.remove(box.right)
                        case None:
                            positions_to_scan.remove(scanned_pos)
        except BreakOuterLoop:
            continue
        robot += inst
        for box in scanned_boxes:
            grid.pop(box.left, None)
            grid.pop(box.right, None)
        for box in scanned_boxes:
            box.left += inst
            box.right += inst
            grid[box.left] = box          
            grid[box.right] = box          

    gps_list = []
    for box in boxes:
        gps_list.append(box.left.real + 100*box.left.imag)
    print("Part 2:", int(sum(gps_list)))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
