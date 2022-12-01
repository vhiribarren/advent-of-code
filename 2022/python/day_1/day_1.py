#!/usr/bin/env python3


INPUT_FILE = "input.txt"


def main():

    calories = []

    with open(INPUT_FILE) as input:
        counter = 0
        for line in input:
            line = line.strip()
            if len(line) == 0:
                calories.append(counter)
                counter = 0
                continue
            counter += int(line)

    calories.sort(reverse=True)

    print("Maximum calory:", calories[0])
    print("Sum of trop three calories:", sum(calories[0:3]))


if __name__ == "__main__":
    main()