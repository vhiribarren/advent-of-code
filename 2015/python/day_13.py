#!/usr/bin/env python3

from pathlib import Path
from collections import defaultdict
from itertools import permutations, pairwise

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def build_happiness(input: str) -> tuple[dict[(str, str), int], set[str]]:
    happiness = defaultdict(int)
    people = set()
    for line in input.splitlines():
        l = line.split()
        p1, p2, direction, cost = l[0], l[-1][:-1], l[2], int(l[3])
        if direction == "lose":
            cost = -cost
        happiness[tuple(sorted([p1, p2]))] += cost
        people.add(p1)
        people.add(p2)
    return happiness, people

def happiness_candidates(input: str):
    happiness, people  = build_happiness(input)
    totals = []
    for arrangement in permutations(people):
        arrangement_happiness = 0
        for steps in pairwise(list(arrangement) + [arrangement[0]]):
            arrangement_happiness += happiness[tuple(sorted(steps))]
        totals.append(arrangement_happiness)
    return totals

def part_1(input_data: str):
    result = max(happiness_candidates(input_data))
    print("Part 1:", result)

def happiness_candidates_with_me(input: str):
    happiness, people  = build_happiness(input)
    people.add("me")
    totals = []
    for arrangement in permutations(people):
        arrangement_happiness = 0
        for steps in pairwise(list(arrangement) + [arrangement[0]]):
            if "me" in steps:
                continue
            arrangement_happiness += happiness[tuple(sorted(steps))]
        totals.append(arrangement_happiness)
    return totals

def part_2(input_data: str):
    result = max(happiness_candidates_with_me(input_data))
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
