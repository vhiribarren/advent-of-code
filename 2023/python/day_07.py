#!/usr/bin/env python3

import os
from collections import Counter

INPUT_FILE = "day_07.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


def part_1(input: str):

    CARD_ORDER = ['A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2']
    CARD_ORDER_MAP = {v: k for k, v in enumerate(CARD_ORDER)}

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
        bucket_hands.sort(key=lambda hand: [CARD_ORDER_MAP[c] for c in list(hand[0])] )
        sorted_hands.extend(bucket_hands)
    sorted_hands.reverse()

    total_winnings = sum([(rank +1) * int(hand[1]) for rank, hand in enumerate(sorted_hands)])
    print("Result part 1:", total_winnings)


def part_2(input: str):

    CARD_ORDER = ['A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J']
    CARD_ORDER_MAP = {v: k for k, v in enumerate(CARD_ORDER)}

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
        j_count = card_count['J']
        del card_count['J']
        most_common = 0 if len(card_count.most_common()) == 0 else card_count.most_common()[0][1]
        most_common += j_count
        snd_most_common =  0 if len(card_count.most_common()) < 2 else card_count.most_common()[1][1]
        match (most_common, snd_most_common):
            case [5, _]: buckets["five_kind"].append(hand)
            case [4, _]: buckets["four_kind"].append(hand)
            case [3, 2]: buckets["full_house"].append(hand)
            case [3, _]: buckets["three_kind"].append(hand)
            case [2, 2]: buckets["two_pair"].append(hand)
            case [2, _]: buckets["one_pair"].append(hand)
            case _: buckets["high_card"].append(hand)

    sorted_hands = []
    for bucket_hands in buckets.values():
        bucket_hands.sort(key=lambda hand: [CARD_ORDER_MAP[c] for c in list(hand[0])] )
        sorted_hands.extend(bucket_hands)
    sorted_hands.reverse()

    total_winnings = sum([(rank +1) * int(hand[1]) for rank, hand in enumerate(sorted_hands)])
    print("Result part 2:", total_winnings)


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)