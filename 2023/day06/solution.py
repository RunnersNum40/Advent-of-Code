from functools import reduce
import os
from typing import List, Tuple
from math import sqrt, floor, ceil
from operator import mul


def read_input(filename: str) -> List[str]:
    """
    Reads the input from the given file using an absolute path and returns a list of each line as a stripped string.
    """
    # Construct the absolute path of the directory where this script is located
    script_dir = os.path.dirname(os.path.abspath(__file__))

    # Combine the script directory with the filename to get the absolute path
    file_path = os.path.join(script_dir, filename)

    with open(file_path, "r") as file:
        return [line.strip() for line in file]


def clean_line(line: str) -> str:
    """Remove the title from a line."""
    return line.split(":")[1].strip()


def process_input(data: List[str]) -> Tuple[str, str]:
    """
    Processes the input data before passing it to the solution functions.
    """
    processed_data = tuple(map(clean_line, data[0:2]))
    return processed_data


def get_ints_from_line(line: str) -> List[int]:
    return list(map(int, line.split()))


def solve_time(race_time: int, record: int) -> int:
    lower_bound = (race_time - sqrt(race_time**2 - 4 * record)) / 2
    upper_bound = (race_time + sqrt(race_time**2 - 4 * record)) / 2

    lower_bound = max(floor(lower_bound + 1), 0)
    upper_bound = min(ceil(upper_bound - 1), race_time)

    return upper_bound - lower_bound + 1


def solve_part_one(lines: Tuple[str, str]) -> int:
    """
    Solves Part One of the day's challenge.
    """
    # Pair numbers in tuples
    races = list(zip(*map(get_ints_from_line, lines)))

    return reduce(
        mul, [solve_time(race_time, record) for race_time, record in races], 1
    )


def solve_part_two(lines: Tuple[str, str]) -> int:
    """
    Solves Part Two of the day's challenge.
    """
    race_time = int(lines[0].replace(" ", ""))
    record = int(lines[1].replace(" ", ""))
    return solve_time(race_time, record)


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
