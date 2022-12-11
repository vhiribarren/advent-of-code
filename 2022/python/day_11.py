#!/usr/bin/env python3

import os
from dataclasses import dataclass
from operator import mul, add

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_11.txt")


@dataclass(init=False)
class Monkey:
    divisible: int
    items: list[int]
    operation: tuple[str, str, str]
    if_true: int
    if_false: int


def parse_input(input: str) -> list[Monkey]:
    monkeys = []
    unparsed_monkeys = [i.split("\n") for i in input.split("\n\n")]
    for m in unparsed_monkeys:
        monkey = Monkey()
        monkey.items = [int(v.strip()) for v in m[1].partition(":")[2].split(",")]
        monkey.operation = tuple(m[2].partition("=")[2].strip().split(" "))
        monkey.divisible = int(m[3].split(" ")[-1])
        monkey.if_true = int(m[4].split(" ")[-1])
        monkey.if_false = int(m[5].split(" ")[-1])
        monkeys.append(monkey)
    return monkeys


def parse_operand(val: str, default: int):
    try:
        return int(val)
    except ValueError:
        return default


def compute_rounds(monkeys: list[Monkey], rounds_nb: int) -> list[int]:
    results: list[int] = [0] * len(monkeys)
    for _ in range(rounds_nb):
        for idx, m in enumerate(monkeys):
            results[idx] += len(m.items)
            match m.operation[1]:
                case "+": op = add
                case "*": op = mul
                case _: raise Exception()
            for item in m.items:
                l, r = parse_operand(m.operation[0], item), parse_operand(m.operation[2], item)
                new_score = op(l, r) // 3
                next_monkey = m.if_true if new_score % m.divisible == 0 else m.if_false
                monkeys[next_monkey].items.append(new_score)
            m.items.clear()
    return results


def main():

    with open(INPUT_FILEPATH) as file_content:
        monkeys = parse_input(file_content.read())

    result = compute_rounds(monkeys, 20)
    result.sort()
    monkey_business = result[-1]*result[-2]
    print(f"Monkey business: {monkey_business}")


if __name__ == "__main__":
    main()