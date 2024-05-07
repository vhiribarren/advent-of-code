#!/usr/bin/env python3

import os

INPUT_FILE = "day_10.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


type Coords = tuple[int, int]

def part_1(input: str):

    network: dict[Coords, set[Coords]] = {}
    start: Coords = None
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            match c:
                case "|": network[(x, y)] = set([(x, y-1), (x, y+1)])
                case "-": network[(x, y)] = set([(x-1, y), (x+1, y)])
                case "L": network[(x, y)] = set([(x, y-1), (x+1, y)])
                case "J": network[(x, y)] = set([(x-1, y), (x, y-1)])
                case "7": network[(x, y)] = set([(x-1, y), (x, y+1)])
                case "F": network[(x, y)] = set([(x+1, y), (x, y+1)])
                case "S": start = (x, y)

    around_start: set[Coords] = set()
    for delta in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
        check = (start[0] + delta[0], start[1] + delta[1])
        if network.get(check) is not None and start in network.get(check):
            around_start.add(check)
    network[start] = around_start

    current_positions = list(around_start)
    routes = [[start, pos] for pos in current_positions]
    continue_search = True
    found_idx = -1
    while continue_search:
        for route_idx in range(len(routes)):
            route = routes[route_idx]
            current_pos = current_positions[route_idx]
            next_pos = list(network[current_pos] - set((route[-2],)))[0]
            if next_pos == start:
                continue_search = False
                found_idx = route_idx
                break
            route.append(next_pos)
            current_positions[route_idx] = next_pos
    good_route = routes[found_idx]
    print("Result Part 1:", len(good_route) // 2)
    return good_route


def part_2(input: str, route: list[Coords]):
    valid_coors = set(route)
    before_start, start, after_start = route[-1], route[0], route[1]
    before_start_diff =  (start[0] - before_start[0], start[1] - before_start[1])
    after_start_diff = (after_start[0] - start[0], after_start[1] - start[1])
    start_type = None
    match before_start_diff, after_start_diff:
        case (1, 0), (1, 0): start_type = "-"
        case (-1, 0), (-1, 0): start_type = "-"
        case (0, 1), (0, 1): start_type = "|"
        case (0, -1), (0, -1): start_type = "|"
        case (-1, 0), (0, 1): start_type = "F"
        case (0, -1), (1, 0): start_type = "F"
        case (1, 0), (0, 1): start_type = "7"
        case (0, -1), (-1, 0): start_type = "7"
        case (0, 1), (1, 0): start_type = "L"
        case (-1, 0), (0, -1): start_type = "L"
        case (1, 0), (0, -1): start_type = "J"
        case (0, 1), (-1, 0): start_type = "J"
    
    is_inside = False
    prev_curve = None
    counter = 0
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            if c == "S":
                c = start_type
            if c != "." and (x, y) not in valid_coors:
                c = "."
            match c:
                case "-": continue
                case "|": is_inside = not is_inside
                case ".": counter += is_inside
                case "L":
                    is_inside = not is_inside
                    prev_curve = "L"
                case "J":
                    if prev_curve == "L":
                        is_inside = not is_inside
                        prev_curve = None
                case "F":
                    is_inside = not is_inside
                    prev_curve = "F"
                case "7":
                    if prev_curve == "F":
                        is_inside = not is_inside
                        prev_curve = None

    print("Result Part 2:", counter)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        route = part_1(input)
        part_2(input, route)