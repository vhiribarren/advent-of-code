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


def find_positions_for_knots_nb(commands: list[tuple[str, int]], knots_nb: int) -> int:
    knots = [(0, 0) for _ in range(knots_nb)]
    tail_positions = {knots[-1]}
    for direction, steps in commands:
        for _ in range(steps):
            head = knots[0]
            tail = knots[1]
            head = (head[0] + direction[0], head[1] + direction[1])
            knots[0] = head
            for idx in range(1, len(knots)):
                tail = knots[idx]
                delta = (head[0] - tail[0], head[1] - tail[1])   
                if 2 not in [abs(d) for d in delta]:
                    head = tail
                    continue
                tail_direction =  [ copysign(1, d)*min(1, max(0, abs(d))) for d in delta]
                tail = (tail[0] + tail_direction[0], tail[1] + tail_direction[1])
                knots[idx] = tail
                head = tail
            tail_positions.add(knots[-1])
    return tail_positions


def main():

    with open(INPUT_FILEPATH) as input:
        commands = [line.strip().split(" ") for line in input.readlines()]
        commands = [(DIRECTIONS[direction], int(step)) for direction, step in commands]

    print("Total positions for 2 knots:", len(find_positions_for_knots_nb(commands, 2)))
    print("Total positions for 10 knots:", len(find_positions_for_knots_nb(commands, 10)))


if __name__ == "__main__":
    main()