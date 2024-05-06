#!/usr/bin/env python3

import os
from dataclasses import dataclass

INPUT_FILE = "day_10.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


type Coords = tuple[int, int]

def part_1(input: str):

    network: dict[Coords, set[Coords]] = {}
    start: Coords = None
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            match c:
                case "|": network[(x, y)] = set([(x, y-1), (x, y+1)])
                case "-": network[(x, y)] = set([(x-1, y), (x+1, y)])
                case "L": network[(x, y)] = set([(x, y-1), (x+1, y)])
                case "J": network[(x, y)] = set([(x-1, y), (x, y-1)])
                case "7": network[(x, y)] = set([(x-1, y), (x, y+1)])
                case "F": network[(x, y)] = set([(x+1, y), (x, y+1)])
                case "S": start = (x, y)

    around_start: set[Coords] = set()
    for delta in [(-1, -1), (-1, 1), (-1, 0), (1, -1), (1, 1), (1, 0), (0, -1), (0, 1)]:
        check = (start[0] + delta[0], start[1] + delta[1])
        if network.get(check) is not None and start in network.get(check):
            around_start.add(check)
    network[start] = around_start

    pos_a, pos_b = list(around_start)
    route_a = [start, pos_a]
    route_b = [start, pos_b]
    while True:
        next_a = list(network[pos_a] - set((route_a[-2],)))[0]
        route_a.append(next_a)
        pos_a = next_a
        next_b = list(network[pos_b] - set((route_b[-2],)))[0]
        route_b.append(next_b)
        pos_b = next_b
        if pos_a == pos_b:
            break
    
    print("Result Part 1:", len(route_a) - 1)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)