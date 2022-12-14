#!/usr/bin/env python3

import os
from copy import deepcopy


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


def sand_algorithm(grid, sand_origin, bottom_pit, bottom_stop):
    iter = 0
    sand_origin_reached = False
    while True:
        iter += 1
        sand = sand_origin
        bottom_reached = False
        while True:
            if bottom_stop and (sand+1j).imag == bottom_pit:
                bottom_reached = True
                iter -= 1
                break
            if (c := sand + 1j) not in grid and c.imag != bottom_pit:
                sand = c
            elif (c := sand -1 + 1j) not in grid and c.imag != bottom_pit:
                sand = c
            elif (c := sand + 1 + 1j) not in grid and c.imag != bottom_pit:
                sand = c
            else:
                grid.add(sand)
                if sand == sand_origin:
                    sand_origin_reached = True
                break
        if bottom_reached or sand_origin_reached:
            break
    return iter


def main():

    with open(INPUT_FILEPATH) as f:
        input = f.read()

    orig_grid = parse_input(input)
    bottom_pit = int(max(orig_grid, key=lambda c: c.imag).imag) + 2
    sand_origin = 500 + 0j

    print("Sand at rest without bottom:", sand_algorithm(deepcopy(orig_grid), sand_origin, bottom_pit, True))
    print("Sand at rest with infinite bottom plan:", sand_algorithm(deepcopy(orig_grid), sand_origin, bottom_pit, False))


if __name__ == "__main__":
    main()