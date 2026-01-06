#!/usr/bin/env python3

from itertools import groupby, islice
from collections import deque

INPUT = "cqjxjnds"

def sliding_window(iterable, n):
    "Collect data into overlapping fixed-length chunks or blocks."
    # sliding_window('ABCDEFG', 4) → ABCD BCDE CDEF DEFG
    iterator = iter(iterable)
    window = deque(islice(iterator, n - 1), maxlen=n)
    for x in iterator:
        window.append(x)
        yield tuple(window)

def has_increasing_letters(password: str) -> bool:
    return any([True for (x, y, z) in sliding_window(map(ord, password), 3) if z == y+1 == x+2])

def has_confusing_letters(password: str) -> bool:
    return any(c in ["i", "o", "l"] for c in password)

def has_2_different_nonoverlapping_pairs(password: str) -> bool:
    return len(list(filter(lambda r: len(list(r[1])) > 1, groupby(password)))) > 1

def inc_password(password: str) -> str:
    password = list(password)
    for idx in range(len(password)-1, -1, -1):
        val = password[idx]
        if val != "z":
            password[idx] = chr(ord(val)+1)
            return "".join(password)
        else:
            password[idx] = "a"

def next_password(password: str) -> str:
    candidate = password
    while True:
        candidate = inc_password(candidate)
        if has_2_different_nonoverlapping_pairs(candidate) \
                and has_increasing_letters(candidate) \
                and not has_confusing_letters(candidate):
            return candidate

def part_1(input_data: str):
    result = next_password(input_data)
    print("Part 1:", result)


def part_2(input_data: str):
    result = next_password(next_password(input_data))
    print("Part 2:", result)


if __name__ == "__main__":
    part_1(INPUT)
    part_2(INPUT)