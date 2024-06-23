import os
from typing import List, Tuple
from number_set import (
    NumberRange,
    range_union,
    range_intersection,
    range_difference,
    NumberSet,
    set_union,
    set_intersection,
    set_difference,
)

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


def map_set(conversion_map: ConversionMap, input_set: NumberSet) -> NumberSet:
    """Map a set using a conversion map.

    Assumes that the conversion map is sorted by input_start
    """
    output_set = NumberSet()

    for output_start, input_start, length in conversion_map:
        input_range = NumberSet([NumberRange(input_start, input_start + length)])
        print("Input", input_range)
        intersection = set_intersection(input_range, input_set)
        intersection.offset(output_start - input_start)

        output_set = set_union(output_set, intersection)
        input_set = set_difference(input_set, input_range)
        print("Difference", input_set)

    print("Mapping", output_set, input_set)
    return set_union(output_set, input_set)


def solve_part_two(almanac_data: Tuple[List[int], List[ConversionMap]]) -> int:
    """Solves Part Two of the day's challenge."""
    initial_seed_ranges, conversion_maps = almanac_data
    number_set = NumberSet(
        [
            NumberRange(start, start + length)
            for start, length in zip(
                initial_seed_ranges[::2], initial_seed_ranges[1::2]
            )
        ]
    )
    for conversion_map in conversion_maps:
        print(number_set)
        number_set = map_set(conversion_map, number_set)

    return min(range.start for range in number_set.ranges)


def main() -> None:
    input_data = read_input("test_input.txt")
    almanac_data = process_input(input_data)

    part_one_solution = solve_part_one(almanac_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(almanac_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
