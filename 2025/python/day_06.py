#!/usr/bin/env python3

from pathlib import Path
from math import prod

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

OPS = {"+": sum, "*": prod}

def part_1(input_data: str):
    lines = input_data.splitlines()
    ops = lines[-1].split()
    nums = [list(map(int, l.split())) for l in lines[:-1]]
    groups = list(zip(*nums))
    results = []
    for op, nums in zip(ops, groups):
        results.append(OPS[op](nums))
    print("Part 1:", sum(results))

def transpose(arr: list[str]) -> list[list[str]]:
    # was: return [[arr[j][i] for j in range(len(arr))] for i in range(len(arr[0]))]
    # learnt this trick on reddit:
    return list(zip(*arr))

def part_2(input_data: str):
    lines = input_data.splitlines()
    ops = lines[-1].split()
    nums = [''.join(l).strip() for l in transpose(lines[:-1])]
    results = []
    group = []
    for n in nums+['']: # Empty char added to process the last group
        if len(n) != 0:
            group.append(int(n))
        else:
            results.append(OPS[ops.pop(0)](group))
            group = []
    print("Part 2:", sum(results))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
