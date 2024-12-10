#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def solver(input: str):
    grid = {complex(x, y): int(c) for y, line in enumerate(input.splitlines()) for x, c in enumerate(line)}
    starts = [coord for coord, height in grid.items() if height == 0]
    score_total = 0
    rating = 0
    for start in starts:
        trails = set[complex]()
        def search_trails(current_pos, current_height, visited):
            nonlocal rating
            new_height = current_height + 1
            for dir in [1, -1, 1j, -1j]:
                new_coord = current_pos + dir
                if new_coord not in grid or new_coord in visited or grid[new_coord] != new_height:
                    continue
                if new_height == 9:
                    trails.add(new_coord)
                    rating += 1
                    continue
                search_trails(new_coord, new_height, visited | {new_coord})
        search_trails(start, 0, {start})
        score_total += len(trails)
    print("Part 1:", score_total)
    print("Part 2:", rating)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        solver(all_input)
