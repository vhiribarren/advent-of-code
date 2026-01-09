#!/usr/bin/env python3

from pathlib import Path
from dataclasses import dataclass

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

TARGET_TIME = 2503

@dataclass
class Reindeer:
    speed: int
    fly_time: int
    rest_time: int

def parse(input_data: str) -> list[Reindeer]:
    return [
        Reindeer(int(w[3]), int(w[6]), int(w[13]))
        for w in (l.split() for l in input_data.splitlines())
    ]

def reindeer_dist(r: Reindeer, time: int) -> int:
    time_chunk = r.fly_time + r.rest_time
    chunk, rest = divmod(time, time_chunk)
    return chunk*r.speed*r.fly_time + r.speed*min(rest, r.fly_time)

def part_1(input_data: str):
    reindeers = parse(input_data)
    distances = [reindeer_dist(r, TARGET_TIME) for r in reindeers]
    result = max(distances)
    print("Part 1:", result)


def part_2(input_data: str):
    reindeers = parse(input_data)
    scores = [0]*len(reindeers)
    for t in range(1, TARGET_TIME):
        distances = [reindeer_dist(r, t) for r in reindeers]
        lead = max(distances)
        scores = [s + (d == lead) for (s, d) in zip(scores, distances)]
    result = max(scores)
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
