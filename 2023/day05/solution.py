import os
from typing import List, Tuple

ConversionMap = List[List[int]]


def read_input(filename: str) -> List[str]:
    """Reads the input from the given file and returns a list of each line as a stripped string."""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, filename)
    with open(file_path, "r") as file:
        return [line.strip() for line in file]


def process_input(data: List[str]) -> Tuple[List[int], List[ConversionMap]]:
    initial_seeds = list(map(int, data[0].split(":")[1].split()))
    conversion_maps = []
    index = 1
    while index < len(data):
        if ":" in data[index]:
            index += 1
            conversion_map = []
            while index < len(data) and data[index] != "":
                conversion_map.append(list(map(int, data[index].split())))
                index += 1
            conversion_maps.append(conversion_map)
            index += 1
        else:
            index += 1
    return initial_seeds, conversion_maps


def map_number(conversion_map: ConversionMap, number: int) -> int:
    """Map a number using a conversion map."""
    for output_start, input_start, length in conversion_map:
        if input_start <= number < input_start + length:
            return output_start + (number - input_start)
    return number


def solve_part_one(almanac_data: Tuple[List[int], List[ConversionMap]]) -> int:
    """Solves Part One of the day's challenge."""
    initial_seeds, conversion_maps = almanac_data
    values = set(initial_seeds)
    for conversion_map in conversion_maps:
        values = {map_number(conversion_map, value) for value in values}
    return min(values)


def main() -> None:
    input_data = read_input("test_input.txt")
    almanac_data = process_input(input_data)

    part_one_solution = solve_part_one(almanac_data)
    print(f"Part One Solution:\n{part_one_solution}")


if __name__ == "__main__":
    main()
