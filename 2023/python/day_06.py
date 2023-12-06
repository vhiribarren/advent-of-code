#!/usr/bin/env python3

import os
from math import sqrt, ceil, floor, prod

INPUT_FILE = "day_06.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

def solver(t_race: int, dist: int) -> int:
    # Quad eq, t*(t_race-t) = dist, t2 - t*t_race + dist = 0
    d = t_race*t_race-4*dist
    t_min = ceil((t_race - sqrt(d)) / 2)
    t_max =  floor((t_race + sqrt(d)) / 2)
    r = t_max - t_min + 1
    if dist == t_min*(t_race-t_min):
        r -= 1
    if dist == t_max*(t_race-t_max):
        r -= 1
    return r

def part_1(input: str):
    lines = input.splitlines()
    times = [int(t) for t in lines[0].split(":")[1].split()]
    distances = [int(d) for d in lines[1].split(":")[1].split()]
    win_ways = []
    for (t_race, dist) in zip(times, distances):
        win_ways.append(solver(t_race, dist))
    print("Result part 1:", prod(win_ways))

def part_2(input: str):
    lines = input.splitlines()
    time = int("".join(lines[0].split(":")[1].split()))
    distance = int("".join(lines[1].split(":")[1].split()))
    print("Result part 2:", solver(time, distance))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)