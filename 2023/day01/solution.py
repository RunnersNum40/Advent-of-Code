import os
from typing import List, Optional, Tuple


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


def process_input(data: List[str]) -> List[str]:
    """
    Processes the input data before passing it to the solution functions.
    """
    processed_data = [line.lower() for line in data]
    return processed_data


def solve_part_one(processed_data: List[str]) -> int:
    """
    Solves Part One of the day's challenge.
    """
    # get all digit characters from the input
    extracted_ints = [
        [int(char) for char in line if char.isdigit()] for line in processed_data
    ]
    # Combine the first and last digits of each line
    calibration_values = [10 * line[0] + line[-1] for line in extracted_ints]
    return sum(calibration_values)


def first_spelled_out_digit(line: str) -> Optional[Tuple[int, int]]:
    """
    Return the first spelled-out digit in the line and its index.
    If none found, returns None.
    """
    digit_map = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    for index, char in enumerate(line):
        for n, digit in enumerate(digit_map, start=1):
            if line.startswith(digit, index):
                return n, index
    return None


def replace_spelled_out_digits(line: str) -> str:
    """
    Replace spelled-out digits in the line with digit characters by their first appearance.
    E.g., 'eightwothree' -> '8wo3'
    """
    while True:
        result = first_spelled_out_digit(line)
        if result is None:
            break
        digit, index = result
        line = line[:index] + str(digit) + line[index + len(str(digit)) :]
    return line


def solve_part_two(processed_data: List[str]) -> int:
    """
    Solves Part Two of the day's challenge.
    """
    # Replace text numbers with digits
    only_digits = [replace_spelled_out_digits(line) for line in processed_data]
    # get all digit characters from the input
    extracted_ints = [
        [int(char) for char in line if char.isdigit()] for line in only_digits
    ]
    # Combine the first and last digits from each line
    calibration_values = [10 * line[0] + line[-1] for line in extracted_ints]
    return sum(calibration_values)


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
