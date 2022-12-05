#!/usr/bin/env python3

import os
import copy

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_5.txt")


def parse_start_position(input):
    stacks = []
    for line in reversed(input[:-1]):
        pos = 1
        idx = 0
        while True:
            try:
                crate = line[pos]
                if len(stacks) <= idx:
                    stacks.append([])
                if crate != " ":
                    stacks[idx].append(crate)
                pos += 4
                idx += 1
            except IndexError:
                break
    return stacks


def main():

    start_position = []
    instructions = []
    with open(INPUT_FILEPATH) as input:
        while (line := input.readline()) != "\n":
            start_position.append(line)
        for line in input:
            raw_inst = line.strip().split(" ")
            instructions.append([int(raw_inst[1]), int(raw_inst[3])-1, int(raw_inst[5])-1 ])

    orig_stacks = parse_start_position(start_position)

    stacks = copy.deepcopy(orig_stacks)
    for inst in instructions:
        count, start, end = inst
        for _ in range(0, count):
            stacks[end].append(stacks[start].pop())
    result = ""
    for stack in stacks:
       result += stack[-1]
    print(result)

    stacks = copy.deepcopy(orig_stacks)
    for inst in instructions:
        count, start, end = inst
        moved_stack = []
        for _ in range(0, count):
            moved_stack.append(stacks[start].pop())
        stacks[end].extend(reversed(moved_stack))
    result = ""
    for stack in stacks:
       result += stack[-1]
    print(result)



if __name__ == "__main__":
    main()