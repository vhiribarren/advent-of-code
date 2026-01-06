#!/usr/bin/env python3

import re
import json
from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def part_1(input_data: str):
    result = sum(map(int, re.findall(r"-?\d+", input_data)))
    print("Part 1:", result)

def compute_sum(jdata):
    match jdata:
        case dict():
            if "red" in jdata.values():
                return 0
            else:
                return sum(compute_sum(l) for l in jdata.values() )
        case list():
            return sum(compute_sum(l) for l in jdata )
        case int():
            return jdata
        case _:
            return 0

def part_2(input_data: str):
    parsed = json.loads(input_data)
    result = compute_sum(parsed)
    print("Part 2:", result)

if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
