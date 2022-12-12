#!/usr/bin/env python3

import os
import math
from dataclasses import dataclass


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_12.txt")

def get_height(height: str):
    match height:
        case "S": return ord("a")
        case "E": return ord("z")
        case val: return ord(val)


def compute_paths(grid, start_coord, end_coords, is_climbing):

    ylen, xlen = len(grid), len(grid[0])
    prev_coords = {start_coord: None}
    dist_coords = {start_coord: 0}
    unvisited_coords = {start_coord}
    visited_coords = set()

    while len(unvisited_coords) != 0:
        min_dist = math.inf
        next_coord = None
        for coord, dist in dist_coords.items():
            if dist < min_dist:
                if coord in unvisited_coords:
                    min_dist = dist
                    next_coord = coord
        x, y = next_coord
        unvisited_coords.remove(next_coord)
        current_height = get_height(grid[y][x])
        new_distance = 1 + dist_coords[(x, y)]
        for dx, dy in [(0, 1), (0, -1), (-1, 0), (1, 0)]:
            xscan = x+dx
            yscan = y+dy
            if 0 <= xscan < xlen and 0 <= yscan < ylen:
                scanned_height = get_height(grid[yscan][xscan])
                is_valid = current_height + 1 >= scanned_height if is_climbing else current_height <= scanned_height + 1
                if is_valid:
                    if (candidate := (xscan, yscan)) not in visited_coords:
                        unvisited_coords.add(candidate)
                    if new_distance < dist_coords.get(candidate, math.inf):
                        prev_coords[candidate] = (x, y)
                        dist_coords[candidate] = new_distance
        visited_coords.add((x, y))
    
    min_steps = math.inf
    for end_coord in end_coords:
        if end_coord not in prev_coords:
            continue
        chain = []
        last_pos = end_coord
        while last_pos is not None:
            chain.append(last_pos)
            last_pos = prev_coords[last_pos]
        steps = len(list(reversed(chain)))-1
        if steps < min_steps:
            min_steps = steps

    return min_steps



def main():

    with open(INPUT_FILEPATH) as file_content:
        grid = [list(s) for s in [l.strip() for l in file_content]]

    ylen, xlen = len(grid), len(grid[0])
    for y in range(ylen):
        for x in range(xlen):
            if grid[y][x] == "S":
                start_coord = (x, y)
            if grid[y][x] == "E":
                end_coord = (x, y)

    print("Best signal steps:", compute_paths(grid, start_coord, [end_coord], True))

    a_positions = []
    for y in range(ylen):
        for x in range(xlen):
            if grid[y][x] == "a" or grid[y][x] == "S":
                a_positions.append((x, y))
    print("Best signal steps:", compute_paths(grid, end_coord, a_positions, False))

if __name__ == "__main__":
    main()