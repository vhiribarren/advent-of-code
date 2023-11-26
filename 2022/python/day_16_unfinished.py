#!/usr/bin/env python3

import os
import re
from dataclasses import dataclass
from typing import Optional


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_16.txt")
RE_PARSER = re.compile(r"^Valve (.*) has flow rate=(.*); tunnels? leads? to valves? (.*)$")


@dataclass(init=False)
class Valve:
    id: str
    rate: int
    next: list[str]
    open: bool = False

@dataclass
class ActionMove:
    from_valve_id: str
    to_valve_index: int

@dataclass
class ActionOpen:
    valve_id: str



def parse_input(input: str) -> dict[str, Valve]:
    valves: dict[str, Valve] = {}
    for l in input.splitlines():
        v = Valve()
        re_result = RE_PARSER.match(l)
        v.id = re_result.group(1)
        v.rate = int(re_result.group(2))
        v.next = [e.strip() for e in re_result.group(3).split(",")]
        valves[v.id] = v
    return valves


def main():

    with open(INPUT_FILEPATH) as f:
        input = f.read()

    valves = parse_input(input)
    root_v = Valve()
    root_v.id = "Root"
    root_v.next = ["AA"]
    valves["Root"] = root_v

    max_iter = 30
    total_rate = 0
    total_pressure = 0
    timer = max_iter
    pressure_candidates = set()
    actions = [ActionMove("Root", 0)]

    def tick_one_minute():
        nonlocal timer, total_rate, total_pressure
        timer -= 1
        total_pressure += total_rate
        print("Minute", max_iter-timer)
    
    def rewind():
        nonlocal timer, total_rate, total_pressure
        while True:
            timer += 1
            total_pressure -= total_rate
            last_action = actions.pop()
            match last_action:
                case ActionMove(prev_v_id, next_index):
                    prev_v = valves[prev_v_id]
                    if next_index < len(prev_v.next)-1:
                        next_index += 1
                        last_action.to_valve_index = next_index
                        actions.append(last_action)
                        break
                case ActionOpen(v_id):
                    v = valves[v_id]
                    v.open = False
                    total_rate -= v.rate
        print("Rewind to minute", max_iter-timer+1)

    try:

        while True:

            if timer < 1:
                actions.pop()
                pressure_candidates.add(total_pressure)
                print("Pressure candidate", total_pressure)
                rewind()
                continue

            action_move = actions[-1]
            prev_v = valves[action_move.from_valve_id]
            v = valves[prev_v.next[action_move.to_valve_index]]

            if not v.open and v.rate > 0:
                tick_one_minute()
                print("Open valve", v.id)
                total_rate += v.rate
                v.open = True
                actions.append(ActionOpen(v.id))
                if timer < 1:
                    actions.pop()
                    pressure_candidates.add(total_pressure)
                    print("Pressure candidate", total_pressure)
                    rewind()
                    continue
                
            tick_one_minute()
            print("Move to valve", v.next[0])
            actions.append(ActionMove(v.id, 0))      
    
    except IndexError as e:
        print("Max possible pressure", max(pressure_candidates))



if __name__ == "__main__":
    main()