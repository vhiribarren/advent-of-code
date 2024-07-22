#!/usr/bin/env python3

import os
from typing import Optional

INPUT_FILE = "day_13.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    result = 0
    row_patterns, col_patterns = zip(*generate_hashed_patterns(input))
    for row_pattern in row_patterns:
        split_idx = search_symetry(row_pattern)
        if split_idx:
            result += 100*split_idx
    for col_pattern in col_patterns:
        split_idx = search_symetry(col_pattern)
        if split_idx:
            result += split_idx
    print("Result 1:", result)


def part_2(input: str):
    ...

def search_symetry(input: list[int]) -> Optional[int]:
    for idx in range(1, len(input)):
        left = input[:idx]
        right = input[idx:]
        left.reverse()
        min_size = min(len(left), len(right)) 
        if left[:min_size] == right[:min_size]:
            return idx
    else:
        return None

def generate_hashed_patterns(input: str) -> list[(list[int], list[int])]:
    hashed_patterns = []
    current_rows = []
    current_cols = []
    current_row_idx = 0
    for line in input.splitlines():
        if len(line) == 0:
            hashed_patterns.append((current_rows, current_cols))
            current_rows = []
            current_cols = []
            current_row_idx = 0
            continue
        for current_col_idx, c in enumerate(line):
            if len(current_rows) <= current_row_idx:
                current_rows.insert(current_row_idx, 0)
            if len(current_cols) <= current_col_idx:
                current_cols.insert(current_col_idx, 0)
            if c == "#":
                current_rows[current_row_idx] += pow(2, current_col_idx)
                current_cols[current_col_idx] += pow(2, current_row_idx)
        current_row_idx += 1
    hashed_patterns.append((current_rows, current_cols))

    return hashed_patterns

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)