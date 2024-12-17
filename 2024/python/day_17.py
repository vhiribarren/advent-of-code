#!/usr/bin/env python3

from pathlib import Path

INPUT_FILEPATH = (p := Path(__file__)).parent/".."/"inputs"/f"{p.stem}.txt"


def combo_deref(arg: int, registers) -> int:
    match arg:
        case v if 0 <= v <= 3: return v 
        case 4: return registers["A"]
        case 5: return registers["B"]
        case 6: return registers["C"]

def part_1(input_data: str):

    registers_raw, program_raw = input_data.split("\n\n")
    registers = {}
    for line in registers_raw.splitlines():
        register_part, value_part = line.split(":")
        registers[register_part.split()[1]] = int(value_part.strip())
    program = [int(v) for v in program_raw.split(":")[1].strip().split(",")]

    ip = 0
    outputs = []
    while ip < len(program):

        inst, arg = program[ip], program[ip + 1]
        match inst:
            case 0: # adv
                registers["A"] = registers["A"] // (2**combo_deref(arg, registers))
                ip += 2
            case 1: # bxl
                registers["B"] = registers["B"] ^ arg
                ip += 2
            case 2: # bst
                registers["B"] = combo_deref(arg, registers) % 8
                ip += 2
            case 3: # jnz
                if registers["A"] == 0:
                    ip += 2
                    continue
                ip = arg
            case 4: # bxc
                registers["B"] = registers["B"] ^ registers["C"]
                ip += 2
            case 5: # out
                out = combo_deref(arg, registers) % 8
                outputs.append(out)
                ip += 2
            case 6: # bdv
                registers["B"] = registers["A"] // (2**combo_deref(arg, registers))
                ip += 2               
            case 7: # cdv
                registers["C"] = registers["A"] // (2**combo_deref(arg, registers))
                ip += 2  

    print(",".join(map(str, outputs)))


def part_2(input_data: str):
    ...


if __name__ == "__main__":
    with INPUT_FILEPATH.open() as file:
        file_data = file.read()
        part_1(file_data)
        part_2(file_data)
