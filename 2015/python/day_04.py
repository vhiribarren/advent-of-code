#!/usr/bin/env python3

import os.path as op
import hashlib

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")

def solver(input: str, zero_nb: int) -> int:
    ans_candidate = 0
    while True:
        test_val = input + str(ans_candidate)
        test_hash = hashlib.md5(test_val.encode()).hexdigest()
        if test_hash[0:zero_nb] == "0" * zero_nb:
            return ans_candidate
        ans_candidate += 1 


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        print("Part 1:", solver(all_input, 5))
        print("Part 2:", solver(all_input, 6))
