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
    def check_cycle():
        nonlocal sig_strength
        if cycle in scanned_cycles:
            print(f"Cycle {cycle} with xreg {xreg}")
            sig_strength += cycle * xreg       
    for inst in input:
        cycle += 1
        check_cycle()
        match inst:
            case ["noop"]:
                pass
            case ["addx", val]:
                cycle += 1
                check_cycle()
                xreg += int(val)
    print("Signal strength:", sig_strength)


if __name__ == "__main__":
    main()