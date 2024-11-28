#!/usr/bin/env python3

import os.path as op
from collections import Counter

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


def has_3_vowels(input: str) -> bool:
    return sum(c in "aeiou" for c in input) >= 3

def has_twice_in_a_row(input: str) -> bool:
    last_val = ""
    for c in input:
        if last_val == c:
            return True
        last_val = c
    return False

def has_bad_words(input: str) -> bool:
    return any(bad_word in input for bad_word in ["ab", "cd", "pq", "xy"])

def part_1(input: str):
    count = 0
    for line in input.splitlines():
        if has_3_vowels(line) and has_twice_in_a_row(line) and not has_bad_words(line):
            count += 1
    print("Part 1:", count)


def has_twice_non_overlapping(input: str) -> bool:
    candidates = Counter()
    input = input[:] + " "
    for idx in range(0, len(input)-2):
        pair = input[idx:idx+2]
        p = pair[0]
        if p == pair[1] and p == input[idx + 2] and p != input[idx + 3]:
            continue
        candidates[pair] += 1
        if candidates[pair] > 1:
            return True
    return False

def has_repeat_char(input: str) -> bool:
    for idx in range(0, len(input) - 2):
        window = input[idx:idx+3]
        if window[0] == window[-1]:
            return True
    return False

def part_2(input: str):
    count = 0
    for line in input.splitlines():
        if has_twice_non_overlapping(line) and has_repeat_char(line):
            count += 1
    print("Part 2:", count)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)