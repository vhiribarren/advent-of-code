#!/usr/bin/env python3

import os
from math import prod

INPUT_FILE = "day_02.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    TARGET = {"red": 12, "green": 13, "blue": 14}
    valid_games = []
    for line in input.splitlines():
        [game, records] = line.split(":")
        game_id = int(game.split()[1])
        valid_entries = []
        for record in [r.strip() for r in records.split(";")]:
            entries = [ l.split() for l in [e.strip() for e in record.split(",")]]
            valid_entries.extend([int(num) <= TARGET[color] for [num, color] in entries])
        if all(valid_entries):
            valid_games.append(game_id)
    print("Result part 1:", sum(valid_games))


def part_2(input: str):
    powers = []
    for line in input.splitlines():
        [_, records] = line.split(":")
        color_draws = { "red": [], "green": [], "blue": [] }
        for record in [r.strip() for r in records.split(";")]:
            entries = [ l.split() for l in [e.strip() for e in record.split(",")]]
            for [num, color] in entries:
                color_draws[color].append(int(num))
        powers.append(prod([max(draws) for _, draws in color_draws.items()]))
    print("Result part 2:", sum(powers))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)