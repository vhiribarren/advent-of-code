#!/usr/bin/env python3

import os
from itertools import repeat

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_8.txt")


def main():

    with open(INPUT_FILEPATH) as input:
        grid = [[int(c) for c in line.strip()] for line in input]
    x_len, y_len = len(grid[0]), len(grid)
    x_range, y_range = range (0, x_len), range (0, y_len)

    scans = []
    scans.extend([zip(repeat(x), y_range) for x in x_range])
    scans.extend([zip(repeat(x), reversed(y_range)) for x in x_range])
    scans.extend([zip(x_range, repeat(y)) for y in y_range])
    scans.extend([zip(reversed(x_range), repeat(y)) for y in y_range])
    visible_trees = set()
    for line_scan in scans:
        min_height = -1
        for x, y in line_scan:
            if (new_height := grid[y][x]) > min_height:
                min_height = new_height
                visible_trees.add((x, y))
    print("Visible trees:", len(visible_trees))

    scenic_scores = []
    for y in y_range:
        for x in x_range:
            scenic_score = 1
            for direction in [(0, 1), (0, -1), (-1, 0), (1, 0)]:
                trees = 0
                idx_x, idx_y = x, y
                base_height = grid[y][x]
                while True:
                    idx_x += direction[0]
                    idx_y += direction[1]
                    if  idx_x < 0 or idx_x >= x_len or idx_y < 0 or idx_y >= y_len:
                        break
                    trees += 1
                    if grid[idx_y][idx_x] >= base_height:
                        break
                scenic_score *= trees
            scenic_scores.append(scenic_score)
    print("Scenic score:", max(scenic_scores))


if __name__ == "__main__":
    main()