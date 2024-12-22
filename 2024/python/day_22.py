#!/usr/bin/env python3

from pathlib import Path
from math import floor
from itertools import pairwise
from dataclasses import dataclass, field
from collections import defaultdict
from typing import cast

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

SECRET_ITERATIONS = 2000


def mix(secret: int, val: int) -> int:
    return secret ^ val

def prune(secret: int) -> int:
    return secret % 16777216

def next_secret(secret: int) -> int:
    secret = prune(mix(secret, secret*64))
    secret = prune(mix(secret, floor(secret/32)))
    secret = prune(mix(secret, secret*2048))
    return secret


def part_1(input_data: str):
    seeds = [int(val) for val in input_data.splitlines()]
    secrets = []
    for seed in seeds:
        secret_round = seed
        for _ in range(SECRET_ITERATIONS):
            secret_round = next_secret(secret_round)
        secrets.append(secret_round)
    print("Part 1:", sum(secrets))


type SeqPrice = tuple[int, int, int, int]

@dataclass(frozen=True)
class Offer:
    buyer: int
    price: int = field(compare=False)
    sequence: SeqPrice

def part_2(input_data: str):
    seeds = [int(val) for val in input_data.splitlines()]
    sequences = []
    for seed in seeds:
        sequence = [seed]
        for _ in range(SECRET_ITERATIONS):
            sequence.append(next_secret(sequence[-1]))
        sequences.append(sequence)
    all_digits = [list(map(lambda x: int(str(x)[-1]), seq)) for seq in sequences]
    all_variations = [[r - l for l, r in pairwise(d)] for d in all_digits]
    all_offers = defaultdict[SeqPrice, set[Offer]](set)
    for buyer_id, buyer_variations in enumerate(all_variations):
        for window_idx in range(len(buyer_variations) - 3):
            offer_seq = tuple(buyer_variations[window_idx:window_idx+4])
            offer_seq = cast(SeqPrice, offer_seq)
            offer = Offer(buyer_id, all_digits[buyer_id][window_idx+3+1], offer_seq)
            all_offers[offer_seq].add(offer) # price is not compared, so only first of each buyer for each range should be put
    bananas = [sum(offer.price for offer in offers)
               for offers in all_offers.values()]
    print("Part 2:", max(bananas))


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
