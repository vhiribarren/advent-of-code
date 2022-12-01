#!/usr/bin/env python3


INPUT_FILE = "input.txt"


def main():

    with open(INPUT_FILE) as input:
        elves_group = input.read().split('\n\n')
        calories = [sum(map(int, elf_group.split())) for elf_group in elves_group ]
        calories.sort(reverse=True)

    print("Maximum calory:", calories[0])
    print("Sum of trop three calories:", sum(calories[0:3]))


if __name__ == "__main__":
    main()