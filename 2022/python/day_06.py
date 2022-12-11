#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_6.txt")


def look_for_n_distinct_chars(n, message):
    for idx in range(0, len(message)-n):
        if len(set(message[idx: idx+n])) == n:
            return idx+n


def main():

    with open(INPUT_FILEPATH) as input:
        message = input.read()
        
    print("Position for 4 chars is", look_for_n_distinct_chars(4, message))
    print("Position for 14 chars is", look_for_n_distinct_chars(14, message))


if __name__ == "__main__":
    main()