#!/usr/bin/env python3

import os
import re
from math import lcm
from itertools import cycle

INPUT_FILE = "day_08.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

NODE_RE = re.compile(r"(\w+) = \((\w+), (\w+)\)")

def part_1(input: str):

    network = {}
    lines = input.splitlines()
    instructions = lines[0]
    for line in lines[2:]:
        m = NODE_RE.match(line)
        network[m.group(1)] = (m.group(2), m.group(3))
    
    counter = 0
    current = "AAA"
    end = "ZZZ"
    for inst in cycle(instructions):
        counter += 1
        current = network[current][0 if inst == "L" else 1]
        if current == end:
            break

    print("Result part 1:", counter)


def part_2(input: str):
    """Using print statements when we reaching Z,
    discovered that actually we have a perfect period for each A start,
    so need to find the common period for all seeds."""

    network = {}
    lines = input.splitlines()
    instructions = lines[0]
    starts = []
    for line in lines[2:]:
        m = NODE_RE.match(line)
        source = m.group(1)
        network[source] = (m.group(2), m.group(3))
        if source.endswith("A"):
            starts.append(source)
    
    counter = 0
    currents = starts[:]
    #print("Starts:", starts)
    periods = [None for _ in range(0, len(starts))]
    first_z = [None for _ in range(0, len(starts))]
    for inst in cycle(instructions):
        counter += 1
        next_dir = 0 if inst == "L" else 1
        for curr_idx, current in enumerate(currents):
            current_next = network[current][next_dir]
            currents[curr_idx] = current_next
            if current_next.endswith("Z"):
                if periods[curr_idx] is None:
                    first_z[curr_idx] = counter
                    #print("First Z "+current_next+" for init: ", starts[curr_idx], "at counter ", counter)
                #if periods[curr_idx] is not None:
                #    print("Found "+current_next+" for init: ", starts[curr_idx], "and period: ", counter - periods[curr_idx])
                if all(periods):
                    break
                periods[curr_idx] = counter
        if all(periods):
            break
        
    print("Result part 2:", lcm(*periods))

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)