#!/usr/bin/env python3

import os
from functools import reduce

INPUT_FILE = "day_15.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

BOX_TOTAL = 256

def part_1(input: str):
    init_seq = input.strip().split(",")
    seq_results = [hash_label(seq) for seq in init_seq]
    print("Part_1", sum(seq_results))

def part_2(input: str):
    init_seq = input.strip().split(",")
    labels: list[list[str]] = [ [] for _ in range(BOX_TOTAL) ]
    lenses: list[dict[str, int]] =  [ dict() for _ in range(BOX_TOTAL) ]
    for curr_seq in init_seq:
        if curr_seq.endswith("-"):
            label = curr_seq[:-1]
            box_id = hash_label(label)
            try:
                labels[box_id].remove(label)
                del lenses[box_id][label]
            except:
                pass
        else:
            label, lense = curr_seq.split("=")
            lense = int(lense)
            box_id = hash_label(label)
            if label not in labels[box_id]:
                labels[box_id].append(label)
            lenses[box_id][label] = lense
    focus_power = 0
    for box_idx in range(BOX_TOTAL):
        for label_idx, label in enumerate(labels[box_idx]):
            focus_power += (box_idx+1)*(label_idx+1)*lenses[box_idx][label]
    print("Part_2", focus_power)

def hash_label(label: str) -> int:
    def hash_alg(acc: str, val: str) -> str:
        acc += ord(val)
        acc *= 17
        acc %= 256
        return acc
    return reduce(hash_alg, label, 0)

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)