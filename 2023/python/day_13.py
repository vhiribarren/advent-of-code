#!/usr/bin/env python3

import os

INPUT_FILE = "day_13.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):
    result = 0
    row_patterns, col_patterns = zip(*generate_hashed_patterns(input))
    for row_pattern in row_patterns:
        split_idx = search_symetry(row_pattern)
        if len(split_idx) > 0:
            result += 100*list(split_idx)[0]
    for col_pattern in col_patterns:
        split_idx = search_symetry(col_pattern)
        if len(split_idx) > 0:
            result += list(split_idx)[0]
    print("Result 1:", result)


def part_2(input: str):
    result = 0
    for (row_pattern, col_pattern) in generate_hashed_patterns(input):
        orig_row_split_idx = search_symetry(row_pattern)
        orig_col_split_idx = search_symetry(col_pattern)

        for row_idx in range(0, len(row_pattern)):
            found = False
            for col_idx in range(0, len(col_pattern)):
                candidate_pattern = row_pattern[:]
                candidate_pattern[row_idx] =  candidate_pattern[row_idx] ^ pow(2, col_idx)
                split_idx = search_symetry(candidate_pattern)
                candidate_idx = split_idx - orig_row_split_idx
                if len(candidate_idx) == 1:
                    result += 100*list(candidate_idx)[0]
                    found = True
                    break
            if found:
                break

        for col_idx in range(0, len(col_pattern)):
            found = False
            for row_idx in range(0, len(row_pattern)):
                candidate_pattern = col_pattern[:]
                candidate_pattern[col_idx] =  candidate_pattern[col_idx] ^ pow(2, row_idx)
                split_idx = search_symetry(candidate_pattern)
                candidate_idx = split_idx - orig_col_split_idx
                if len(candidate_idx) == 1:
                    result += list(candidate_idx)[0]
                    found = True
                    break
            if found:
                break

    print("Result 2:", result)


def search_symetry(input: list[int]) -> set[int]:
    result = set()
    for idx in range(1, len(input)):
        left = input[:idx]
        right = input[idx:]
        left.reverse()
        min_size = min(len(left), len(right))
        if left[:min_size] == right[:min_size]:
            result.add(idx)
    return result


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