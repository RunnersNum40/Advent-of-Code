import os
from typing import List


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


def process_input(data: List[str]):
    """
    Processes the input data before passing it to the solution functions.
    """
    # Process the data as needed for your solution
    processed_data = data
    return processed_data


def solve_part_one(processed_data) -> int:
    """
    Solves Part One of the day's challenge.
    """
    # Implement the solution for Part One
    return None


def solve_part_two(processed_data) -> int:
    """
    Solves Part Two of the day's challenge.
    """
    # Implement the solution for Part Two
    return None


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
