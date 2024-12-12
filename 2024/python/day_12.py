#!/usr/bin/env python3

from pathlib import Path
from collections import Counter

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def create_regions(grid: dict[complex, str]) -> list[(str, set[complex])]:
    regions = []
    while len(grid) != 0:
        region = []
        coord, plant = next(iter(grid.items()))
        def consume_region(plant: str, coord: complex):
            nonlocal region
            region.append(coord)
            del grid[coord]
            for dir in [-1, 1, -1j, 1j]:
                next_coord = coord + dir
                if next_coord in grid and grid[next_coord] == plant:
                    consume_region(plant, next_coord)
        consume_region(plant, coord)
        regions.append((plant, region))
    return regions

def compute_area(region: set[complex]) -> int:
    counter = Counter()
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
def compute_sides(region: set[complex]) -> int:
    counter = Counter()
    directions = dict()
    for coord in region:
        for delta in [-0.5, 0.5, -0.5j, 0.5j]:
            barrier = coord + delta
            counter[barrier] += 1
            directions[barrier] = 2*delta
    borders = {c for c in counter if counter[c] == 1}
    same_rows = list[set[complex]]()
    same_columns = list[set[complex]]()
    while len(borders) > 0:
        border = borders.pop()
        same_line = {border}
        if border.real.is_integer():
            for candidate_border in borders.copy():
                if candidate_border.imag == border.imag:
                    same_line.add(candidate_border)
                    borders.remove(candidate_border)
            same_line = sorted(same_line, key=lambda x: x.real)
            same_rows.append(same_line)
        else:
            for candidate_border in borders.copy():
                if candidate_border.real == border.real:
                    same_line.add(candidate_border)
                    borders.remove(candidate_border)
            same_line = sorted(same_line, key=lambda x: x.imag)
            same_columns.append(same_line)
    total = 0
    for row in same_rows:
        prec_val = -100
        for i in range(0, len(row)):
            if row[i].real != prec_val.real +1 or directions[row[i]] != directions[prec_val]:
                total += 1
            prec_val = row[i]
    for column in same_columns:
        prec_val = - 100j
        for i in range(0, len(column)):
            if column[i].imag != prec_val.imag +1 or directions[column[i]] != directions[prec_val]:
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
