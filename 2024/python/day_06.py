#!/usr/bin/env python3

from pathlib import Path
from itertools import product

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def parser(input: str) -> tuple[complex, set[complex], int, int]:
    obstacles: set[complex] = set()
    for (y, line) in enumerate(input.splitlines()):
        for (x, c) in enumerate(line):
            match c:
                case "#":
                    obstacles.add(complex(x, y))
                case "^":
                    guard_pos = complex(x, y)
    return (guard_pos, obstacles, x+1, y+1)


def solver(input: str):
    guard_pos_init, obstacles_init, width, height = parser(input)

    guard_pos, obstacles = guard_pos_init, set(obstacles_init)
    direction = -1j
    visited_part1: set[complex] = set()
    while True:
        visited_part1.add(guard_pos)
        next_pos = guard_pos + direction
        if next_pos in obstacles:
            direction *= 1j
        elif next_pos.real < 0 or next_pos.real >= width\
            or next_pos.imag < 0 or next_pos.imag >= height:
            break
        else:
            guard_pos = next_pos
    print("Part 1:", len(visited_part1))

    obstructions_count = 0
    for obst_new in visited_part1:

        if obst_new in obstacles_init or obst_new == guard_pos_init:
            continue
        direction = -1j
        visited: set[tuple[complex, complex]] = set() # position, direction
        guard_pos = guard_pos_init
        obstacles = obstacles_init | {obst_new}

        while True:
            if (guard_pos, direction) in visited:
                obstructions_count += 1
                break
            visited.add((guard_pos, direction))
            next_pos = guard_pos + direction
            if next_pos in obstacles:
                direction *= 1j
            elif next_pos.real < 0 or next_pos.real >= width\
                or next_pos.imag < 0 or next_pos.imag >= height:
                break
            else:
                guard_pos = next_pos
    print("Part 2:", obstructions_count)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        solver(all_input)