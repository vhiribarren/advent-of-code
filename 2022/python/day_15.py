#!/usr/bin/env python3

import os
import re
import math
from dataclasses import dataclass
from typing import Optional


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_15.txt")
RE_PARSER = re.compile(r"x=(-?\d+), y=(-?\d+):.*x=(-?\d+), y=(-?\d+)$")


@dataclass
class Triple:
    sensor: complex
    beacon: complex
    dist: int


def manhattan_dist(l: complex, r: complex) -> int:
    return int(abs(l.real - r.real)+abs(l.imag - r.imag))


def range_for_triple(t: Triple, *, x:int = None, y: int=None) -> Optional[list[int]]:
    if x is not None:
        delta_x = abs(t.sensor.real-x)
        if delta_x > t.dist:
            return None
        delta_y = t.dist - delta_x
        return [t.sensor.imag-delta_y, t.sensor.imag+delta_y]
    if y is not None:
        delta_y = abs(t.sensor.imag-y)
        if delta_y > t.dist:
            return None
        delta_x = t.dist - delta_y
        return [t.sensor.real-delta_x, t.sensor.real+delta_x]


def scan_all(triples: list[Triple], min: int, max: int) -> complex:
    for y in range(min, max+1):
        if y % 1000 == 0:
            print("Scanning line", y)
        result = scan_line(triples, y, min, max)
        if len(result) == 1:
            return result[0]


def scan_line(triples: list[Triple], y: int, min_col: int, max_col: int) -> list[complex]:
    uncovered_points = []
    covered_ranges = []
    for t in triples:
        if (r := range_for_triple(t, y=y)) is not None:
            covered_ranges.append(r)
    x = min_col
    while True:
        if x > max_col:
            break
        for l, r in covered_ranges:
            if l <= x <= r:
                x = r+1
                break
        else:
            uncovered_points.append(complex(x, y))
            x += 1
    return uncovered_points


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
    no_beacon_count = max_x - min_x + 1 - len(scan_line(triples, y, min_x, max_x))
    found_beacons = set()
    for t in triples:
        if t.beacon.imag == y:
            found_beacons.add(t.beacon)
    no_beacon_count -= len(found_beacons)
    print("No beacon count: ", no_beacon_count)

    mysterious_beacon = scan_all(triples, 0, 4000000)
    print("Tuning frequency:", int(mysterious_beacon.real*4000000 + mysterious_beacon.imag))


if __name__ == "__main__":
    main()