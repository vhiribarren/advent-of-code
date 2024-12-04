#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input: str):
    puzzle = input.splitlines()
    width = len(puzzle)
    to_be_reverted_lines = []
    for i in range(0, width):
        to_be_reverted_lines.append("".join(puzzle[j][i] for j in range(0, width)))
        to_be_reverted_lines.append("".join(puzzle[i+j][j] for  j in range(0, width - i)))
        to_be_reverted_lines.append("".join(puzzle[i+j][width-1-j] for j in range(0, width - i)))
        if i != 0:
            to_be_reverted_lines.append("".join(puzzle[j][i+j] for  j in range(0, width - i)))
            to_be_reverted_lines.append("".join(puzzle[j][width-1-j-i] for  j in range(0, width - i)))
    words = puzzle[:]
    words.extend([line[::-1] for line in puzzle])
    words.extend(to_be_reverted_lines[:])
    words.extend([line[::-1] for line in to_be_reverted_lines])
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