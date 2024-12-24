#!/usr/bin/env python3

from pathlib import Path
from dataclasses import dataclass
from typing import Callable
from operator import xor, and_, or_
from functools import cache

INPUT_FILEPATH = (p := Path(__file__)).parent / ".." / "inputs" / f"{p.stem}.txt"

ACTION_MAP = {"AND": and_, "XOR": xor, "OR": or_}


@dataclass
class Op:
    op1: str
    op2: str
    action: Callable[[int, int], int]


def part_1(input_data: str):
    starts_raw, connections_raw = input_data.split("\n\n")
    starts = {
        label: int(value.strip())
        for label, value in (start.split(":") for start in starts_raw.splitlines())
    }
    connections = {
        res: Op(op1, op2, ACTION_MAP[action_raw])
        for op1, action_raw, op2, _, res in (
            connections.split() for connections in connections_raw.splitlines()
        )
    }
    finals = {key for key in connections.keys() if key.startswith("z")}

    @cache
    def compute_node(label: str) -> int:
        if label in starts:
            return starts[label]
        node = connections[label]
        return node.action(compute_node(node.op1), compute_node(node.op2))

    print("Part 1:", sum(compute_node(final) << int(final[1:]) for final in finals))


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
