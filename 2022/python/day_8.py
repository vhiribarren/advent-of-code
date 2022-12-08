#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_8.txt")


def main():

    with open(INPUT_FILEPATH) as input:
        grid = [[int(c) for c in line.strip()] for line in input]
    x_len, y_len = len(grid[0]), len(grid)
    x_range, y_range = range (0, x_len), range (0, y_len)

    visible_trees = set()
    def check_and_add_tree():
        nonlocal min_height
        if (new_height := grid[y][x]) > min_height:
            min_height = new_height
            visible_trees.add((x, y))
    for y in y_range:
        for x_scan_pos in [x_range, reversed(x_range)]:
            min_height = -1
            for x in x_scan_pos:
                check_and_add_tree()
    for x in x_range:
        for y_scan_pos in [y_range, reversed(y_range)]:        
            min_height = -1
            for y in y_scan_pos:
                check_and_add_tree()
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