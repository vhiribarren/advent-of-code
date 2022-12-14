#!/usr/bin/env python3

import os


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_14.txt")


def parse_input(input):
    grid = set()
    for l in input.splitlines():
        rock_coords = [[int(v) for v in c.split(",")] for c in l.split(" -> ")]
        for i in range(len(rock_coords)-1):
            w = rock_coords[i:i+2]
            src = complex(w[0][0], w[0][1])
            dst = complex(w[1][0], w[1][1])
            delta = dst-src
            if delta.real == 0:
                for imag in range(int(min(src.imag, dst.imag)), int(max(src.imag, dst.imag))+1):
                    grid.add(complex(src.real, imag))
            else:
                for real in range(int(min(src.real, dst.real)), int(max(src.real, dst.real))+1):
                    grid.add(complex(real, src.imag))
    return grid


def main():

    with open(INPUT_FILEPATH) as f:
        input = f.read()

    grid = parse_input(input)
    bottom_pit = int(max(grid, key=lambda c: c.imag).imag)
    sand_origin = 500 + 0j

    iter = 0
    while True:
        iter += 1
        sand = sand_origin
        abyss_reached = False
        while True:
            if (c := sand + 1j) not in grid:
                sand = c
            elif (c := sand -1 + 1j) not in grid:
                sand = c
            elif (c := sand + 1 + 1j) not in grid:
                sand = c
            else:
                grid.add(sand)
                break
            if sand.imag > bottom_pit:
                abyss_reached = True
                break
        if abyss_reached:
            break
    print("Sand at rest:", iter-1)


if __name__ == "__main__":
    main()
