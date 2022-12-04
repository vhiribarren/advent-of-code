#!/usr/bin/env python3


INPUT_FILENAME = "input.txt"


def main():

    set_inputs = []
    with open(INPUT_FILENAME) as input:
        set_inputs = []
        for line in input:
            parsed_input = [elf.split("-") for elf in line.strip().split(",")]
            set_input = [set(range(int(x), int(y)+1)) for [x, y] in parsed_input]
            set_inputs.append(set_input)

    count_included_ranges = sum([True for l, r in set_inputs if l <= r or r <= l])
    print(count_included_ranges)

    count_overlap_ranges = sum([True for s in set_inputs if set.intersection(*s)])
    print(count_overlap_ranges)


if __name__ == "__main__":
    main()