#!/usr/bin/env python3

import os

INPUT_FILE = "day_04.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)

def part_1(input: str):
    points = []
    for line in input.splitlines():
        [winnings, draws] = line.split(":")[1].split("|")
        winnings = set(winnings.split())
        draws = draws.split()
        has_won = [d in winnings for d in draws]
        points.append(2**(s-1) if (s := sum(has_won)) > 0 else 0)
    print("Result part 1:", sum(points))

def part_2(input: str):
    cards: dict[int, int] = {}
    for line in input.splitlines():
        [card, game] = line.split(":")
        card_id = int(card.split()[1])
        cards[card_id] = cards.get(card_id, 0) + 1;
        card_id_total = cards[card_id]
        [winnings, draws] = game.split("|")
        winnings = set(winnings.split())
        draws = draws.split()
        has_won = [d in winnings for d in draws]
        for i in range(0, sum(has_won)):
            new_id = card_id + i + 1
            cards[new_id] = cards.get(new_id, 0) + card_id_total;
    print("Result part 2:", sum(cards.values()))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)