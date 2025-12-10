#!/usr/bin/env python3

"""
Template for Advent of Code.
"""

from pathlib import Path
from collections import deque

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"

type Machine = tuple[bool, ...]
type Button = set[int]
type Buttons = list[Button]
type Jolts = list[int]
type Problem = tuple[Machine, Buttons, Jolts]
type Count = int
type ButtonIndex = int
type Visited = set[Machine]


def parse_input(input_data: str) -> list[Problem]:
    problems = list[Problem]()
    for line in input_data.splitlines():
        buttons_start_idx = line.index("]")+1
        jolt_start_idx = line.index("{")
        machines = tuple([c == "#" for c in line[1:buttons_start_idx-1]])
        buttons = [set(map(int, b[1:-1].split(","))) for b in line[buttons_start_idx+1: jolt_start_idx-1].split()]
        jolts = list(map(int, line[jolt_start_idx+1:-1].split(",")))
        problems.append((machines, buttons, jolts))
    return problems


def run_machine(machine: Machine, button: Button) -> Machine:
    mut_machine = list(machine)
    for idx in button:
        mut_machine[idx] = not mut_machine[idx]
    return tuple(mut_machine)


def solver_1(target: Machine, buttons: Buttons) -> Count:
    search_queue = deque[tuple[Machine, Visited, list[ButtonIndex]]]()
    machine_start = tuple([False]*len(target))
    search_queue.append((machine_start, set(), []))
    while True:
        last_machine, visited, btn_indices = search_queue.pop()
        if last_machine == target:
            return len(btn_indices)
        visited |= {last_machine}
        for btn_idx, button in enumerate(buttons):
            next_machine = run_machine(last_machine, button)
            if next_machine in visited:
                continue
            search_queue.appendleft((next_machine, visited, btn_indices[:]+[btn_idx]))


def part_1(input_data: str):
    problems = parse_input(input_data)
    result = sum([solver_1(machine, buttons) for (machine, buttons, _) in problems])
    print("Part 1:", result)


def part_2(input_data: str):
    result = None
    print("Part 2:", result)


if __name__ == "__main__":
    file_data = INPUT_FILEPATH.read_text()
    part_1(file_data)
    part_2(file_data)
