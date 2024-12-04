#!/usr/bin/env python3

import os.path as op

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


# Quite complex, no time to do simpler for now
def part_1(input: str):
    puzzle = input.splitlines()
    width = len(puzzle)
    words = puzzle[:]
    words.extend([line[::-1] for line in puzzle])
    vertical_words = []
    for i in range(0, width):
        vertical_words.append("".join(puzzle[j][i] for j in range(0, width)))
    words.extend(vertical_words[:])
    words.extend([line[::-1] for line in vertical_words])
    diag_words = []
    for i in range(0, width):
        diag_words.append("".join(puzzle[i+j][j] for  j in range(0, width - i)))
        diag_words.append("".join(puzzle[i+j][width-1-j] for j in range(0, width - i)))
        if i != 0:
            diag_words.append("".join(puzzle[j][i+j] for  j in range(0, width - i)))
            diag_words.append("".join(puzzle[j][width-1-j-i] for  j in range(0, width - i)))
    words.extend(diag_words[:])
    words.extend([line[::-1] for line in diag_words])
    print("Part 1:", sum([w.count("XMAS") for w in words]))


def part_2(input: str):
    puzzle = input.splitlines()
    width = len(puzzle)
    count = 0
    for i in range(1, width -1):
        for j in range(1, width -1):
            if puzzle[i][j] == "A"\
                    and {puzzle[i-1][j-1], puzzle[i+1][j+1]} == {"M", "S"}\
                    and {puzzle[i-1][j+1], puzzle[i+1][j-1]} == {"M", "S"}:
                count += 1
    print("Part 2:", count)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        part_1(all_input)
        part_2(all_input)