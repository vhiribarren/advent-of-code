#!/usr/bin/env python3

import math
import heapq
from pathlib import Path
from dataclasses import dataclass
from itertools import count
from typing import Optional

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

WIDTH = 71
HEIGHT = 71
TAKE = 1024

COUNTER = count()


@dataclass
class Node:
    score = math.inf
    coord: complex
    prec: Optional["Node"] = None


def search_path(obstacles: list[complex]) -> Optional[int]:
    """
    Dijkstra's algorithm to find the shortest path.
    """
    maze = {complex(x, y): Node(complex(x, y)) for x in range(0, WIDTH) for y in range(0, HEIGHT) if complex(x, y) not in obstacles}
    start_node = maze[0]
    start_node.score = 0
    end_node = maze[complex(WIDTH-1, HEIGHT-1)]

    unvisited = [(node.score, next(COUNTER), node) for node in maze.values()]
    heapq.heapify(unvisited)
    visited = []

    while len(unvisited) > 0:
        _, _, current_node = heapq.heappop(unvisited)
        if current_node == end_node:
            if current_node.score == math.inf:
                return None
            return int(current_node.score)
        for direction in [1, -1j, -1, 1j]:
            next_node = maze.get(current_node.coord + direction)
            if next_node is None or next_node in visited:
                continue
            next_node_score = current_node.score +1
            if next_node_score < next_node.score:
                next_node.prec = current_node
                next_node.score = next_node_score
                heapq.heappush(unvisited, (next_node.score, next(COUNTER), next_node))
        visited.append(current_node)

    return None


def part_1(input_data: str):
    obstacles = [complex(int(x), int(y)) for x, y in
             (coord.split(",") for coord in input_data.splitlines())]
    print("Part 1", search_path(obstacles[:TAKE]))


def part_2(input_data: str):
    """
    Bisect algorithm to find fastly the index when the path is closed.
    """
    obstacles = [complex(int(x), int(y)) for x, y in
             (coord.split(",") for coord in input_data.splitlines())]
    range_min = 0
    range_max = len(obstacles)
    while range_min < range_max:
        range_middle = range_min + (range_max - range_min)//2
        print("Testing", range_middle)
        if search_path(obstacles[:range_middle]) is None:
            range_max = range_middle
        else:
            range_min = range_middle +1
    print("Part 2:", input_data.splitlines()[range_min-1])


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
