#!/usr/bin/env python3


INPUT_FILENAME = "input.txt"


def item_score(char):
    char_ord = ord(char)
    if ord('A') <= char_ord <= ord('Z'):
        return char_ord - ord('A') + 27
    elif ord('a') <= char_ord <= ord('z'):
        return char_ord - ord('a') + 1
    else:
        raise Exception("Should not happen")


def main():

    with open(INPUT_FILENAME) as input:
        priority = 0
        for line in input:
            line.strip()
            compartment_1 = line[:len(line)//2]
            compartment_2 = line[len(line)//2:]
            common_char = list(set.intersection(set(compartment_1), set(compartment_2)))[0]
            priority += item_score(common_char)
        print(priority)

    with open(INPUT_FILENAME) as input:
        group_counter = 0;
        group = []
        priority = 0
        for line in input:
            group.append(line.strip())
            group_counter = (group_counter + 1) % 3
            if group_counter != 0:
                continue
            sets = [set(l) for l in group]
            common_element = list(set.intersection(*sets))[0]
            priority += item_score(common_element)
            group = []
        print(priority)


if __name__ == "__main__":
    main()