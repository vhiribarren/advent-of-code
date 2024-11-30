#!/usr/bin/env python3

import os.path as op
from enum import Enum, auto
from dataclasses import dataclass
from typing import Optional
from functools import cache

INPUT_FILEPATH = op.join(op.dirname(__file__), "..", "inputs", f"{op.splitext(op.basename(__file__))[0]}.txt")


class Op(Enum):
    Assign = auto()
    And = auto()
    Or = auto()
    RShift = auto()
    LShift = auto()
    Not = auto()

@dataclass
class Instruction:
    op: Op
    in_1: str
    in_2: Optional[str] = None

INSTS: dict[str, Instruction] = dict()

def parser(input: str) -> dict[str, Instruction]:
    result = dict()
    for line in input.splitlines():
        match l:= line.split():
            case ["NOT", *_]:
                result[l[3]] = Instruction(Op.Not, l[1])
            case [_, "->", *_]: 
                result[l[2]] = Instruction(Op.Assign, l[0])
            case [_, o, _, "->", *_]:
                op = None
                match o:
                    case "AND": op = Op.And
                    case "OR": op = Op.Or
                    case "RSHIFT": op = Op.RShift
                    case "LSHIFT": op = Op.LShift
                result[l[4]] = Instruction(op, l[0], l[2])
    return result

@cache
def eval_output(wire: str) -> int:
    try:
        return int(wire)
    except:
        pass
    inst = INSTS[wire]
    match inst.op:
        case Op.Assign:
            try:
                return int(inst.in_1)
            except:
                return eval_output(inst.in_1)
        case Op.Not:
            return ~eval_output(inst.in_1)
        case Op.And:
            return eval_output(inst.in_1) & eval_output(inst.in_2)
        case Op.Or:
            return eval_output(inst.in_1) | eval_output(inst.in_2)
        case Op.RShift:
            return eval_output(inst.in_1) >> eval_output(inst.in_2)
        case Op.LShift:
            return eval_output(inst.in_1) << eval_output(inst.in_2)

def solver(input: str):
    global INSTS
    INSTS = parser(input)
    signal = eval_output("a")
    print("Part 1:", signal)
    eval_output.cache_clear()
    INSTS["b"] = Instruction(Op.Assign, str(signal))
    print("Part 2:", eval_output("a"))


if __name__ == "__main__":
    with open(INPUT_FILEPATH) as input:
        all_input = input.read()
        solver(all_input)