#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_8.txt")



def main():

    with open(INPUT_FILEPATH) as input:
        grid = [[int(c) for c in line.strip()] for line in input]

    visible_trees = set()
    for y in range(0, len(grid)):
        min_height = -1
        for x in range (0, len(grid[0])):
            if (new_height := grid[y][x]) > min_height:
                min_height = new_height
                visible_trees.add((x, y))

        min_height = -1
        for x in reversed(range (0, len(grid[0]))):
            if (new_height := grid[y][x]) > min_height:
                min_height = new_height
                visible_trees.add((x, y))
    for x in range(0, len(grid[0])):
        min_height = -1
        for y in range (0, len(grid)):
            if (new_height := grid[y][x]) > min_height:
                min_height = new_height
                visible_trees.add((x, y))

        min_height = -1
        for y in reversed(range (0, len(grid))):
            if (new_height := grid[y][x]) > min_height:
                min_height = new_height
                visible_trees.add((x, y))
    print(len(visible_trees))

    scenic_scores = []
    for y in range(0, len(grid)):
        for x in range(0, len(grid[0])):
            scenic_score = 1
            for direction in [(0, 1), (0, -1), (-1, 0), (1, 0)]:
                trees = 0
                idx_x = x
                idx_y = y
                min_height = grid[y][x]
                while True:
                    idx_x += direction[0]
                    idx_y += direction[1]
                    if  idx_x < 0 or idx_x >= len(grid[0]) or idx_y < 0 or idx_y >= len(grid):
                        break
                    trees += 1
                    if (new_height := grid[idx_y][idx_x]) >= min_height:
                        break
                scenic_score *= trees
            scenic_scores.append(scenic_score)
    print(max(scenic_scores))

if __name__ == "__main__":
    main()