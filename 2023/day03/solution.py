import os
from typing import List, Tuple, Set


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


def process_input(data: List[str]) -> List[List[str]]:
    """
    Processes the input data before passing it to the solution functions.
    """
    # Process the data as needed for your solution
    processed_data = [list(line) for line in data]
    return processed_data


def is_symbol(char: str) -> bool:
    """Checks if a character is a symbol (not a period or a digit)."""
    return not char.isdigit() and char != "."


def find_start_of_number(schematic: List[List[str]], row: int, col: int) -> int:
    """
    Finds the start column index of a number given any digit within it.
    """
    while col > 0 and schematic[row][col - 1].isdigit():
        col -= 1
    return col


def read_number(schematic: List[List[str]], row: int, col: int) -> Tuple[int, str]:
    """
    Reads a number from the schematic starting from a given position.

    Returns the number and the points that make it up.
    """
    cols = len(schematic[0])
    start_col = find_start_of_number(schematic, row, col)
    number, id_parts = "", []

    for c in range(start_col, cols):
        if schematic[row][c].isdigit():
            number += schematic[row][c]
            id_parts.append(f"{row},{c}")
        else:
            break

    return int(number), "-".join(id_parts)


def find_neighbors(schematic: List[List[str]], row: int, col: int) -> Set[int]:
    """
    Finds all unique neighboring numbers of a given symbol in the schematic.
    """
    neighbors = [(-1, 0), (0, -1), (1, 0), (0, 1), (-1, -1), (-1, 1), (1, -1), (1, 1)]
    neighbor_numbers = set()

    for dr, dc in neighbors:
        r, c = row + dr, col + dc
        # If a char is a symbol
        if (
            0 <= r < len(schematic)
            and 0 <= c < len(schematic[0])
            and schematic[r][c].isdigit()
        ):
            # Find it's unique part numbers
            number, _ = read_number(schematic, r, c)
            neighbor_numbers.add(number)

    return neighbor_numbers


def solve_part_one(schematic: List[List[str]]) -> int:
    """
    Solves Part One of the challenge by finding the sum of all unique numbers adjacent to symbols.
    """
    unique_numbers = set()

    for row_index, row in enumerate(schematic):
        for col_index, char in enumerate(row):
            if is_symbol(char):
                unique_numbers.update(find_neighbors(schematic, row_index, col_index))

    return sum(unique_numbers)


def solve_part_two(schematic: List[List[str]]) -> int:
    """
    Solves Part Two of the challenge by finding the sum of gear ratios for all gears.
    """
    total_sum = 0

    for row_index, row in enumerate(schematic):
        for col_index, char in enumerate(row):
            if char == "*":
                neighbors = find_neighbors(schematic, row_index, col_index)

                if len(neighbors) == 2:
                    total_sum += neighbors.pop() * neighbors.pop()

    return total_sum


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
