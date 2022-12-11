#!/usr/bin/env python3

from __future__ import annotations
import os


INPUT_FILEPATH = os.path.join(os.path.dirname(__file__), "../inputs/day_7.txt")


class File:
    def __init__(self, name: str, size: int) -> None:
        self.name = name
        self.size = size

class Directory:
    def __init__(self, name: str, parent: Directory = None) -> None:
        self.name = name
        self.parent = parent
        self.files = {}


def parse_fs(input: list[str]) -> Directory:
    fs = Directory("/")
    current_root = fs

    def parse_cd():
        nonlocal current_root
        match inst[2]:
            case "/":
                current_root = fs
            case "..":
                current_root = current_root.parent
            case dirname:
                if not dirname in current_root.files:
                    current_root.files[dirname] = Directory(dirname, current_root)
                current_root = current_root.files[dirname]
    
    def parse_ls():
        nonlocal idx
        while idx+1 < len(input) and not input[idx+1].startswith("$"):
            idx += 1
            file_info = input[idx].split(" ")
            match file_info[0]:
                case "dir":
                    dir = Directory(file_info[1], current_root)
                    current_root.files[dir.name] = dir
                case _:
                    file = File(file_info[1], int(file_info[0]))
                    current_root.files[file.name] = file

    idx = 0
    while idx < len(input):
        inst = input[idx].split()
        if inst[0] != "$":
            raise Exception("Should not happen")
        match inst[1]:
            case "cd": parse_cd()
            case "ls": parse_ls()
        idx += 1

    return fs


def compute_dir_sizes(fs):
    results = []
    def root_size(root: Directory) -> int:
        nonlocal results
        size = 0
        for file in root.files.values():
            match file:
                case File(size=s):
                    size += s
                case Directory():
                    size += root_size(file)
        results.append((root, size))
        return size
    root_size(fs)
    return results


def main():

    with open(INPUT_FILEPATH) as input:
        input_lines = [l.strip() for l in input.readlines()]
    
    fs = parse_fs(input_lines)
    dir_sizes = compute_dir_sizes(fs)
    print(sum([size for dir, size in dir_sizes if size < 100000]))

    dir_sizes.sort(key=lambda v: v[1])
    used_space = dir_sizes[-1][1]
    free_space = 70000000 - used_space
    needed_space = 30000000 - free_space
    for _, size in dir_sizes:
        if size > needed_space:
            print(size)
            break


if __name__ == "__main__":
    main()