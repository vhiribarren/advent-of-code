#!/usr/bin/env python3

from pathlib import Path
from math import floor

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def mix(secret: int, val: int) -> int:
    return secret ^ val

def prune(secret: int) -> int:
    return secret % 16777216

def next_secret(secret: int) -> int:
    secret = prune(mix(secret, secret*64))
    secret = prune(mix(secret, floor(secret/32)))
    secret = prune(mix(secret, secret*2048))
    return secret


def part_1(input_data: str):
    seeds = [int(val) for val in input_data.splitlines()]
    secrets = []
    for seed in seeds:
        secret_round = seed
        for _ in range(2000):
            secret_round = next_secret(secret_round)
        secrets.append(secret_round)
    print("Part 1:", sum(secrets))


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
