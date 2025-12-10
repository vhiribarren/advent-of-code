#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.13"
# dependencies = [
#     "shapely",
# ]
# ///

from pathlib import Path
from itertools import combinations
import shapely

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/"day_09.txt"


def part_1(input_data: str):
    tiles = [tuple(map(int, l.split(","))) for l in input_data.splitlines()]
    areas = [
        (1 + abs(l[0] - r[0])) * (1 + abs(l[1] - r[1]))
        for (l, r) in combinations(tiles, 2)
    ]
    result = max(areas)
    print("Part 1:", result)

# Usage of Shapely for now
# I have various ideas but nothing easy to implement
def part_2(input_data: str):
    tiles = [tuple(map(int, l.split(","))) for l in input_data.splitlines()]
    polygon = shapely.Polygon(tiles)
    areas = [
        (1 + abs(l[0] - r[0])) * (1 + abs(l[1] - r[1]))
        for (l, r) in combinations(tiles, 2)
        if polygon.contains(shapely.box(*l, *r))
    ]
    result = max(areas)
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
