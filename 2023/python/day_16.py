#!/usr/bin/env python3

import os
from enum import StrEnum, auto, Flag
from typing import NamedTuple, assert_never

INPUT_FILE = "day_16.txt"
INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "..", "inputs", INPUT_FILE)


class Element(StrEnum):
    MirrorSlash = "/"
    MirrorSlashReverse = "\\"
    SplitterHorizontal = "-"
    SplitterVertical = "|"

class Direction(Flag):
    Left = auto()
    Right = auto()
    Up = auto()
    Down = auto()

class Beam(NamedTuple):
    pos: complex
    dir: Direction
    def next_pos(self):
        match self.dir:
            case Direction.Left: return self.pos - 1
            case Direction.Right: return self.pos + 1
            case Direction.Up: return self.pos - 1j
            case Direction.Down: return self.pos + 1j
            case _: assert_never() 

def mirror_slash_effect(current_dir: Direction) -> Direction:
    match current_dir:
        case Direction.Left: return Direction.Down
        case Direction.Right: return Direction.Up
        case Direction.Up: return Direction.Right
        case Direction.Down: return Direction.Left

def mirror_slash_reverse_effect(current_dir: Direction) -> Direction:
    match current_dir:
        case Direction.Left: return Direction.Up
        case Direction.Right: return Direction.Down
        case Direction.Up: return Direction.Left
        case Direction.Down: return Direction.Right


def part_1(input: str):
    print("Part 1:", beam_field(*extract_elements(input), Beam(-1+0j, Direction.Right)))

def part_2(input: str):
    elements, width, height = extract_elements(input)
    score_candidates = []
    for x in range(width):
        score_candidates.append(beam_field(elements, width, height, Beam(x-1j, Direction.Down)))
        score_candidates.append(beam_field(elements, width, height, Beam(x+height*1j, Direction.Up)))
    for y in range(height):
        score_candidates.append(beam_field(elements, width, height, Beam(-1+y*1j, Direction.Right)))
        score_candidates.append(beam_field(elements, width, height, Beam(width+y*1j, Direction.Left)))
    print("Part 2:", max(score_candidates))

def beam_field(elements, width, height, init_beam: Beam) -> int:
    tiles: dict[complex, Direction] = {}
    paths = [init_beam]
    def splitterHorizontalEffect():
        nonlocal curr_beam
        match curr_beam.dir:
            case Direction.Left | Direction.Right:
                curr_beam = Beam(next_beam_pos, curr_beam.dir)
            case Direction.Up | Direction.Down:
                paths.extend([Beam(next_beam_pos, Direction.Left), Beam(next_beam_pos, Direction.Right)])
                curr_beam = None
    def splitterVerticalEffect():
        nonlocal curr_beam
        match curr_beam.dir:
            case Direction.Up | Direction.Down:
                curr_beam = Beam(next_beam_pos, curr_beam.dir)
            case Direction.Left | Direction.Right:
                paths.extend([Beam(next_beam_pos, Direction.Up), Beam(next_beam_pos, Direction.Down)])
                curr_beam = None
    while len(paths) > 0:
        curr_beam = paths.pop()
        while curr_beam != None:
            next_beam_pos = curr_beam.next_pos()
            if not (0 <= next_beam_pos.real < width and 0 <= next_beam_pos.imag < height):
                break
            tile_energy = tiles.get(next_beam_pos, Direction(0))
            if curr_beam.dir in tile_energy:
                break
            tiles[next_beam_pos] = tile_energy | curr_beam.dir
            match elements.get(next_beam_pos):
                case Element.MirrorSlash:
                    curr_beam = Beam(next_beam_pos, mirror_slash_effect(curr_beam.dir))
                case Element.MirrorSlashReverse:
                    curr_beam = Beam(next_beam_pos, mirror_slash_reverse_effect(curr_beam.dir))
                case Element.SplitterHorizontal:
                    splitterHorizontalEffect()
                case Element.SplitterVertical:
                    splitterVerticalEffect()
                case _:
                    curr_beam = Beam(next_beam_pos, curr_beam.dir)
    return len(tiles)


def extract_elements(input: str) -> dict[complex, Element]:
    elements = {}
    for y, line in enumerate(input.splitlines()):
        for x, c in enumerate(line):
            coords = complex(x, y)
            try:
                elements[coords] = Element(c)
            except:
                pass
    return elements, x+1, y+1

if __name__ == "__main__":
    with open(INPUT_FILEPATH) as f:
        input = f.read()
        part_1(input)
        part_2(input)