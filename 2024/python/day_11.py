#!/usr/bin/env python3

from pathlib import Path
from functools import cache

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


@cache
def convert_stone(stone: int) -> tuple[int, ...]:
        if stone == 0:
            return (1,)
        elif (stone_len := len(stone_str := str(stone))) % 2 == 0:
            return(int(stone_str[0:stone_len//2]), int(stone_str[stone_len//2:]))
        else:
            return (stone * 2024,)

@cache
def compute_len(stone: int, iter_nb: int) -> int:
    if iter_nb == 0:
        return 1
    return sum(compute_len(new_stone, iter_nb -1) for new_stone in convert_stone(stone))

def solver(input: str, iter_nb: int) -> int:
    init = list(map(int, input.split()))
    return sum(compute_len(stone, iter_nb) for stone in init)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        print("Part 1:", solver(all_input, 25))
        print("Part 2:", solver(all_input, 75))