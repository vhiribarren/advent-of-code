#!/usr/bin/env python3

from pathlib import Path
from itertools import permutations, pairwise

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

def build_distances(input: str) -> tuple[dict[(str, str), int], set[str]]:
    distances = dict()
    towns = set()
    for line in input.splitlines():
        l = line.split()
        distances[tuple(sorted([l[0], l[2]]))] = int(l[4])
        towns.add(l[0])
        towns.add(l[2])
    return distances, towns

def distance_candidates(input: str):
    distances, towns  = build_distances(input)
    totals = []
    for trip in permutations(towns):
        trip_distance = 0
        for steps in pairwise(trip):
            trip_distance += distances[tuple(sorted(steps))]
        totals.append(trip_distance)
    return totals

def part_1(input: str):
    print("Result 1:", min(distance_candidates(input)))

def part_2(input: str):
    print("Result 2:", max(distance_candidates(input)))

if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)