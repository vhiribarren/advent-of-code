#!/usr/bin/env python3

import os

INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_2.txt")

BATTLE_POINTS_1 = {
    ("A", "X"): 3 + 1,
    ("A", "Y"): 6 + 2,
    ("A", "Z"): 0 + 3,
    ("B", "X"): 0 + 1,
    ("B", "Y"): 3 + 2,
    ("B", "Z"): 6 + 3,  
    ("C", "X"): 6 + 1,
    ("C", "Y"): 0 + 2,
    ("C", "Z"): 3 + 3, 
}

BATTLE_POINTS_2 = {
    ("A", "X"): 0 + 3,
    ("A", "Y"): 3 + 1,
    ("A", "Z"): 6 + 2,
    ("B", "X"): 0 + 1,
    ("B", "Y"): 3 + 2,
    ("B", "Z"): 6 + 3,  
    ("C", "X"): 0 + 2,
    ("C", "Y"): 3 + 3,
    ("C", "Z"): 6 + 1,    
}


def main():

    with open(INPUT_FILEPATH) as input: 
        rounds = [tuple(line.strip().split()) for line in input]

    score_1 = sum([BATTLE_POINTS_1[round] for round in rounds])
    print(score_1)

    score_2 = sum([BATTLE_POINTS_2[round] for round in rounds])
    print(score_2)


if __name__ == "__main__":
    main()