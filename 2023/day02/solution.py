import os
from typing import List, Dict


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


def process_input(data: List[str]) -> List[List[Dict[str, int]]]:
    """
    Processes the input data before passing it to the solution functions.
    """
    processed_data = []
    for line in data:
        # Split the input into game Ids and results
        id, result = line.split(":")
        # Split the results into sets of cubes
        cube_sets = [set.strip().split(",") for set in result.split(";")]
        # Convert the revealed numbers into integers
        game_results = []
        for set in cube_sets:
            cube_quantities = {"red": 0, "green": 0, "blue": 0}
            for color in set:
                quantity, index_color = color.strip().split(" ")
                cube_quantities[index_color] = int(quantity)
            game_results.append(cube_quantities)
        processed_data.append(game_results)
    return processed_data


def is_valid_game(game: List[dict]) -> bool:
    """Returns True if the given game is valid, False otherwise."""
    max_cubes = {"red": 12, "green": 13, "blue": 14}
    for set in game:
        if any([set[color] > max_cubes[color] for color in set]):
            return False

    return True


def solve_part_one(processed_data: List[List[Dict[str, int]]]) -> int:
    """
    Solves Part One of the day's challenge.
    """
    # Find games that do no exceed the maximum number of cubes
    valid_games = [
        id for id, result in enumerate(processed_data, start=1) if is_valid_game(result)
    ]
    return sum(valid_games)


def find_minimum_cubes(game: List[Dict[str, int]]) -> Dict[str, int]:
    """Returns the minimum number of cubes required to make a valid game."""
    min_cubes = {"red": 0, "green": 0, "blue": 0}
    for set in game:
        for color in set:
            min_cubes[color] = max(min_cubes[color], set[color])
    return min_cubes


def solve_part_two(processed_data: List[List[Dict[str, int]]]) -> int:
    """
    Solves Part Two of the day's challenge.
    """
    # Find the minimum number of cubes for each game
    min_cubes = [find_minimum_cubes(game) for game in processed_data]
    # Find the power of each game
    powers = [game["red"] * game["green"] * game["blue"] for game in min_cubes]
    return sum(powers)


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
