#!/usr/bin/env python3

from math import prod
from pathlib import Path
from itertools import combinations

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

MAX_JUNCTIONS = 1000
MAX_CIRCUITS = 3


def dist_square(left, right):
    return (left[0] - right[0])**2 + (left[1] - right[1])**2 + (left[2] - right[2])**2


def part_1(input_data: str):
    junctions = [tuple(map(int, line.split(","))) for line in input_data.splitlines()]
    pairs = []
    for (l, r) in combinations(junctions, 2):
        pairs.append((dist_square(l, r), l, r))
    pairs.sort(key=lambda p: p[0])
    groups = []
    for (_, l, r) in pairs[:MAX_JUNCTIONS]:
        target_idx = [idx for idx, group in enumerate(groups) if l in group or r in group]
        if len(target_idx) == 0:
            groups.append({l, r})
        elif len(target_idx) == 1:
            groups[target_idx[0]] |= {l, r}
        else:
            fst_grp = groups.pop(target_idx[1]) # pop in reverse order!!
            snd_grp = groups.pop(target_idx[0])
            groups.append(fst_grp | snd_grp | {l, r})
    box_sizes = sorted([len(g) for g in groups], reverse=True)
    result = prod(box_sizes[:MAX_CIRCUITS])
    print("Part 1:", result)


def part_2(input_data: str):
    junctions = [tuple(map(int, line.split(","))) for line in input_data.splitlines()]
    pairs = []
    for (l, r) in combinations(junctions, 2):
        pairs.append((dist_square(l, r), l, r))
    pairs.sort(key=lambda p: p[0])
    groups = []
    for (_, l, r) in pairs:
        target_idx = [idx for idx, group in enumerate(groups) if l in group or r in group]
        if len(target_idx) == 0:
            groups.append({l, r})
        elif len(target_idx) == 1:
            groups[target_idx[0]] |= {l, r}
        else:
            fst_grp = groups.pop(target_idx[1]) # pop in reverse order!!
            snd_grp = groups.pop(target_idx[0])
            groups.append(fst_grp | snd_grp | {l, r})
        if len(groups[0]) == len(junctions):
            break
    result = l[0] * r[0]
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
