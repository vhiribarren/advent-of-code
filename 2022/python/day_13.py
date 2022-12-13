#!/usr/bin/env python3

import os
import json
from enum import IntEnum
import functools


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_13.txt")


class Order(IntEnum):
    RIGHT = -1
    CONTINUE = 0
    NOT_RIGHT = 1


def is_right_order(left, right) -> Order:
    match (left, right):
        case (list(), list()):
            for idx in range(len(left)):
                if idx > len(right)-1:
                    return Order.NOT_RIGHT
                l = left[idx]
                r = right[idx]
                match order := is_right_order(l, r):
                    case Order.RIGHT| Order.NOT_RIGHT: return order
                    case Order.CONTINUE:
                        continue
            else:
                if len(left) == len(right):
                    return Order.CONTINUE
                else:
                    return Order.RIGHT
        case (int(), list()):
            return is_right_order([left], right)
        case (list(), int()):
            return is_right_order(left, [right])
        case (int(), int()):
            if left < right:
                return Order.RIGHT
            elif left > right:
                return Order.NOT_RIGHT
            else:
                return Order.CONTINUE

def main():

    with open(INPUT_FILEPATH) as f:
        input = f.read()
    pairs = [g.split("\n") for g in input.split("\n\n") ]
    pairs = [[json.loads(v) for v in pair] for pair in pairs]
    signals = [v for pair in pairs for v in pair]

    results = []
    for idx, (l, r) in enumerate(pairs):
        if is_right_order(l, r) == Order.RIGHT:
            results.append(idx + 1)
    print("Result of sum", sum(results))

    signals.append([[2]])
    signals.append([[6]])
    signals.sort(key=functools.cmp_to_key(is_right_order))
    div_idx_1 = signals.index([[2]])+1
    div_idx_2 = signals.index([[6]])+1
    print("Key", div_idx_1*div_idx_2)


if __name__ == "__main__":
    main()
