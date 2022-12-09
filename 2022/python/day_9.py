#!/usr/bin/env python3

import os
from math import copysign

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_9.txt")


DIRECTIONS = {
    "R": (1, 0),
    "L": (-1, 0),
    "U": (0, 1),
    "D": (0, -1),
}

def main():

    with open(INPUT_FILEPATH) as input:
        commands = [line.strip().split(" ") for line in input.readlines()]
        commands = [(DIRECTIONS[direction], int(step)) for direction, step in commands]

    head, tail = (0, 0), (0, 0)
    positions = {tail}

    for direction, steps in commands:
        for _ in range(steps):
            head = (head[0] + direction[0], head[1] + direction[1])
            delta = (head[0] - tail[0], head[1] - tail[1])   
            if 2 not in [abs(d) for d in delta]:
                continue
            tail_direction =  [ copysign(1, d)*min(1, max(0, abs(d))) for d in delta]
            tail = (tail[0] + tail_direction[0], tail[1] + tail_direction[1])
            positions.add(tail)
    print("Total positions:", len(positions))


if __name__ == "__main__":
    main()