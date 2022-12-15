#!/usr/bin/env python3

import os
import re
import math
from dataclasses import dataclass


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_15.txt")
RE_PARSER = re.compile(r"x=(-?\d+), y=(-?\d+):.*x=(-?\d+), y=(-?\d+)$")


@dataclass
class Triple:
    sensor: complex
    beacon: complex
    dist: int


def manhattan_dist(l: complex, r: complex) -> int:
    return int(abs(l.real - r.real)+abs(l.imag - r.imag))


def main():

    with open(INPUT_FILEPATH) as f:
        input = f.read()

    triples: list[Triple] = []
    for l in input.splitlines():
        re_result = RE_PARSER.search(l)
        sensor = complex(int(re_result.group(1)), int(re_result.group(2)))
        beacon = complex(int(re_result.group(3)), int(re_result.group(4)))
        dist = manhattan_dist(sensor, beacon)
        triples.append(Triple(sensor, beacon, dist))

    max_x = -math.inf
    min_x = math.inf
    for t in triples:
        if (c := t.sensor.real + t.dist) > max_x:
            max_x = int(c)
        if (c := t.sensor.real - t.dist) < min_x:
            min_x = int(c)

    y = 2000000
    no_beacon_count = 0
    for x in range(min_x, max_x+1):
        scan_coord = complex(x, y)
        for t in triples:
            if manhattan_dist(scan_coord, t.sensor) <= t.dist:
                no_beacon_count += 1
                break
    found_beacons = set()
    for t in triples:
        if t.beacon.imag == y:
            found_beacons.add(t.beacon)
    no_beacon_count -= len(found_beacons)
    print(no_beacon_count)



if __name__ == "__main__":
    main()