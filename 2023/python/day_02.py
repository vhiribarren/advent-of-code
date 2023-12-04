#!/usr/bin/env python3

import os
import re
from math import prod

INPUT_FILE = "day_02.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def parse_line(line: str) -> (int, list[tuple[int, str]]):
    [game, records_txt] = line.split(":")
    game_id = int(game.split()[1])
    records = [r.split() for r in re.split(r";|,", records_txt)]
    return (game_id, records)

def part_1(input: str):
    TARGET = {"red": 12, "green": 13, "blue": 14}
    valid_games = []
    for line in input.splitlines():
        (game_id, records) = parse_line(line)
        valid_entries = []
        valid_entries.extend([int(num) <= TARGET[color] for [num, color] in records])
        if all(valid_entries):
            valid_games.append(game_id)
    print("Result part 1:", sum(valid_games))

def part_2(input: str):
    powers = []
    for line in input.splitlines():
        (_, records) = parse_line(line)
        color_draws = { "red": [], "green": [], "blue": [] }
        for [num, color] in records:
            color_draws[color].append(int(num))
        powers.append(prod([max(draws) for _, draws in color_draws.items()]))
    print("Result part 2:", sum(powers))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)