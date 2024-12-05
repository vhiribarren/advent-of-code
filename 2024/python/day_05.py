#!/usr/bin/env python3

from pathlib import Path
from dataclasses import dataclass
from functools import cmp_to_key

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


@dataclass
class Either:
    left: set[str]
    right: set[str]


def solver(input: str):
    rules: dict[str, Either] = dict()
    raw_rules, raw_updates = input.split("\n\n")
    for raw_rule in raw_rules.splitlines():
        l, r = raw_rule.split("|")
        if rules.get(l) is None:
            rules[l] = Either(set(), set())
        if rules.get(r) is None:
            rules[r] = Either(set(), set())
        rules[l].right.add(r)
        rules[r].left.add(l)

    sum_correct = 0
    incorrect_updates = []
    for raw_update in raw_updates.splitlines():
        update = raw_update.split(",")
        for idx, page in enumerate(update):
            if set(update[:idx]) <= rules[page].left and set(update[idx+1:]) <= rules[page].right:
                continue
            incorrect_updates.append(update)
            break
        else:
            sum_correct += int(update[len(update)//2])
    print("Part 1:", sum_correct)

    sum_incorrect = 0
    def sort_func(x, y):
        return 1 if x in rules[y].left else -1
    for update in incorrect_updates:
        sorted_update = sorted(update, key=cmp_to_key(sort_func))
        sum_incorrect += int(sorted_update[len(sorted_update)//2])
    print("Part 2:", sum_incorrect)



if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        solver(all_input)