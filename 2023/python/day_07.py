#!/usr/bin/env python3

import os
from collections import Counter

INPUT_FILE = "day_07.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

CARD_ORDER = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
CARD_ORDER_MAP = {v: k for k, v in enumerate(CARD_ORDER)}

def part_1(input: str):

    hands = [line.split() for line in input.splitlines()]
    buckets = {
        "five_kind": [],
        "four_kind": [],
        "full_house": [],
        "three_kind": [],
        "two_pair": [],
        "one_pair": [],
        "high_card": [],
    }

    for hand in hands:
        card_count = Counter(hand[0])
        match card_count.most_common():
            case [(_, 5), *_]: buckets["five_kind"].append(hand)
            case [(_, 4), *_]: buckets["four_kind"].append(hand)
            case [(_, 3), (_, 2)]: buckets["full_house"].append(hand)
            case [(_, 3), *_]: buckets["three_kind"].append(hand)
            case [(_, 2), (_, 2), *_]: buckets["two_pair"].append(hand)
            case [(_, 2), *_]: buckets["one_pair"].append(hand)
            case _: buckets["high_card"].append(hand)

    sorted_hands = []
    for bucket_hands in buckets.values():
        if len(bucket_hands) == 0:
            continue
        bucket_hands.sort(key=lambda hand: [CARD_ORDER_MAP[c] for c in list(hand[0])] )
        sorted_hands.extend(bucket_hands)
    sorted_hands.reverse()

    total_winnings = sum([(rank +1) * int(hand[1]) for rank, hand in enumerate(sorted_hands)])
    print("Result part 1:", total_winnings)


def part_2(input: str):
    ...





if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)