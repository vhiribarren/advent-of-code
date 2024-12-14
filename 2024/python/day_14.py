#!/usr/bin/env python3

from pathlib import Path
from dataclasses import dataclass
import math
from collections import Counter

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

WIDTH = 101
HEIGHT = 103
ITERATIONS = 100
MIRRORED_COEFF =  0.2

@dataclass
class Robot:
    pos: complex
    speed: complex


def parser(input_data: str) -> list[Robot]:
    robots = list[Robot]()
    for line in input_data.splitlines():
        pos = complex(*list(map(int, line.split()[0].split("=")[1].split(","))))
        speed =  complex(*list(map(int, line.split()[1].split("=")[1].split(","))))
        robots.append(Robot(pos, speed))
    return robots

def part_1(input_data: str):
    robots = parser(input_data)
    quadrants = [0, 0, 0, 0]
    for robot in robots:
        x = (robot.pos.real + robot.speed.real*ITERATIONS) % WIDTH
        y = (robot.pos.imag + robot.speed.imag*ITERATIONS) % HEIGHT
        if x < WIDTH // 2 and y < HEIGHT // 2:
            quadrants[0] += 1
        elif x < WIDTH // 2 and y > HEIGHT // 2:
            quadrants[1] += 1
        elif x > WIDTH // 2 and y > HEIGHT // 2:
            quadrants[2] += 1
        elif x > WIDTH // 2 and y < HEIGHT // 2:
            quadrants[3] += 1
    print("Part 1:", math.prod(quadrants))


def display(robots: list[Robot]):
    positions = {r.pos for r in robots}
    for y in range(HEIGHT):
        for x in range(WIDTH):
            if complex(x, y) in positions:
                print("#", end="")
            else:
                print(".", end="")
        print()


def part_2(input_data: str):
    """
    We will suppose the tree is symetric and is centered. The part 1 could
    suggest that. We can try to find if all robots have a mirror, but not in the
    central lines.
    Moreover, "most of the robots arrange themselves". Unfortunately, it means
    that it covers only a proportion of robots. So if we have an unusually high
    number of robots having a symetric part, probably we reach the moment
    we have the christmas tree.
    """
    robots = parser(input_data)
    seconds = 0
    while True:
        seconds += 1
        for robot in robots:
            robot.pos = complex(
                (robot.pos.real + robot.speed.real) % WIDTH,
                (robot.pos.imag + robot.speed.imag) % HEIGHT)
        counter = Counter[complex]()
        total_mirrored = 0
        for robot in robots:
            if robot.pos.real == WIDTH // 2:
                continue
            counter[robot.pos] += 1
        for position in counter:
            if counter[position] == counter[complex( WIDTH - position.real -1, position.imag )]:
                total_mirrored += 1
        if total_mirrored > len(robots) * MIRRORED_COEFF:
            break
    print(f"Part 2: after {seconds} seconds, {total_mirrored}/{len(robots)} robot are mirrored")
    display(robots)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
