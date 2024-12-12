#!/usr/bin/env python3

from pathlib import Path
from collections import Counter

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def create_regions(grid: dict[complex, str]) -> list[tuple[str, list[complex]]]:
    regions = []
    while len(grid) != 0:
        region = []
        coord, plant = next(iter(grid.items()))
        def consume_region(plant: str, coord: complex):
            nonlocal region
            region.append(coord)
            del grid[coord]
            for direction in [-1, 1, -1j, 1j]:
                next_coord = coord + direction
                if next_coord in grid and grid[next_coord] == plant:
                    consume_region(plant, next_coord)
        consume_region(plant, coord)
        regions.append((plant, region))
    return regions

def compute_area(region: list[complex]) -> int:
    counter = Counter[complex]()
    for coord in region:
        for delta in [-0.5, 0.5, -0.5j, 0.5j]:
            counter[coord + delta] += 1
    return sum([1 for c in counter if counter[c] == 1])

def part_1(input_data: str):
    grid = {complex(x, y): c for y, line in enumerate(input_data.splitlines()) for x, c in enumerate(line)}
    regions = create_regions(grid)
    perimeters = [len(region) for _, region in regions]
    area = [compute_area(region) for _, region in regions]
    print("Part 1:", sum(p * a for p, a in zip(perimeters, area)))

# ...kof...kof....... absolutely need to review this absolute monstruosity
def compute_sides(region: list[complex]) -> int:
    counter = Counter[complex]()
    directions = dict[complex, complex]()
    for coord in region:
        for delta in [-0.5, 0.5, -0.5j, 0.5j]:
            barrier = coord + delta
            counter[barrier] += 1
            directions[barrier] = 2*delta
    borders = {c for c in counter if counter[c] == 1}

    same_rows = list[list[complex]]()
    same_columns = list[list[complex]]()
    while borders:
        border = borders.pop()
        line = {border}
        if border.real.is_integer():    # Horizontal direction
            line.update({b for b in borders if b.imag == border.imag})
            same_rows.append(sorted(line, key=lambda x: x.real))
        else:                           # Vertical direction
            line.update({b for b in borders if b.real == border.real})
            same_columns.append(sorted(line, key=lambda x: x.imag))
        borders.difference_update(line)

    total = 0
    for row in same_rows:
        prec_val = None
        for i in range(len(row)):
            if prec_val is None or row[i].real != prec_val.real +1 or directions[row[i]] != directions[prec_val]:
                total += 1
            prec_val = row[i]
    for column in same_columns:
        prec_val = None
        for i in range(len(column)):
            if prec_val is None or column[i].imag != prec_val.imag +1 or directions[column[i]] != directions[prec_val]:
                total += 1
            prec_val = column[i]
    return total

def part_2(input_data: str):
    grid = {complex(x, y): c for y, line in enumerate(input_data.splitlines()) for x, c in enumerate(line)}
    regions = create_regions(grid)
    sides = [len(region) for _, region in regions]
    area = [compute_sides(region) for _, region in regions]
    print("Part 2:", sum(s * a for s, a in zip(sides, area)))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
