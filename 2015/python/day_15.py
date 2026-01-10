#!/usr/bin/env python3

from pathlib import Path
from math import prod

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"
TOTAL_CALORIES = 500
TOTAL_SPOON = 100

type Ingredient = list[int]

def parser(input_data: str) -> list[Ingredient]:
    return [
        list(map(int, [cap[:-1], dur[:-1], flav[:-1], tex[:-1], cal]))
        for _, _, cap, _, dur, _, flav, _, tex, _, cal
        in (line.split() for line in input_data.splitlines())
    ]

def compositions(total: int, n: int) -> tuple[int, ...]:
    if n == 1:
        yield (total, )
    else:
        for x in range(total+1):
            for rest in compositions(total - x, n - 1):
                yield (x,) + rest

def solver(input_data: str, with_calories: bool):
    ingredients = parser(input_data)
    prop_num = len(ingredients[0])-1
    scores = []
    for spoons in compositions(TOTAL_SPOON, prop_num):
        quantities = [[spoon*p for p in props] for (spoon, props) in zip(spoons, ingredients)]
        sum_quantities = [max(v, 0) for v in map(sum, zip(*quantities))]
        if with_calories and sum_quantities[-1] != TOTAL_CALORIES:
            continue
        scores.append(prod(sum_quantities[:-1]))
    return max(scores)

def part_1(input_data: str):
    print("Part 1:", solver(input_data, False))

def part_2(input_data: str):
    print("Part 1:", solver(input_data, True))


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
