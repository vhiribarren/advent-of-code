#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_1.txt")


def main():

    with open(INPUT_FILEPATH) as input:
        elves_group = input.read().split('\n\n')
        calories = [sum(map(int, elf_group.split())) for elf_group in elves_group ]
        calories.sort(reverse=True)

    print("Maximum calory:", calories[0])
    print("Sum of trop three calories:", sum(calories[0:3]))


if __name__ == "__main__":
    main()