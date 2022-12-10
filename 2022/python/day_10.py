#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_10.txt")


def main():

    with open(INPUT_FILEPATH) as file_content:
        input = [l.strip().split() for l in file_content]
    
    scanned_cycles = set(range(20, 220+1, 40))
    xreg = 1
    cycle = 0
    sig_strength = 0
    def next_cycle():
        nonlocal sig_strength, cycle
        cycle += 1
        if cycle in scanned_cycles:
            sig_strength += cycle * xreg       
    for inst in input:
        next_cycle()
        match inst:
            case ["noop"]:
                pass
            case ["addx", val]:
                next_cycle()
                xreg += int(val)
    print("Signal strength:", sig_strength)

    LINE_SIZE = 40
    xreg = 1
    cycle = 0
    def update_and_draw_cycle():
        nonlocal sig_strength, cycle
        line_pos = cycle % LINE_SIZE
        if line_pos == 0:
            print("")
        if xreg -1 <= line_pos <= xreg + 1:
            print("#", end="")
        else:
            print(".", end="")
        cycle += 1
    for inst in input:
        update_and_draw_cycle()
        match inst:
            case ["noop"]:
                pass
            case ["addx", val]:
                update_and_draw_cycle()
                xreg += int(val)
    print("")

if __name__ == "__main__":
    main()