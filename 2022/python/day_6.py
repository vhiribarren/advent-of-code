#!/usr/bin/env python3

import os
from collections import deque

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_6.txt")


def look_for_n_distinct_chars(n, message):
    window = deque(message[:n])
    idx = n-1
    while True:
        if len(set(window)) == n:
            break
        idx += 1
        window.popleft()
        window.append(message[idx])
    return idx+1


def main():

    with open(INPUT_FILEPATH) as input:
        message = input.read()
        
    print("Position for 4 chars is", look_for_n_distinct_chars(4, message))
    print("Position for 14 chars is", look_for_n_distinct_chars(14, message))


if __name__ == "__main__":
    main()