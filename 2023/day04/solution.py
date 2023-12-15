import os
from typing import List, Tuple, Any, Iterable
from functools import lru_cache


def read_input(filename: str) -> List[str]:
    """Reads the input from the given file using an absolute path and returns a list of each line as a stripped string."""
    script_dir = os.path.dirname(os.path.abspath(__file__))
    file_path = os.path.join(script_dir, filename)
    with open(file_path, "r") as file:
        return [line.strip() for line in file]


def read_number_list(line: str) -> Tuple[int, ...]:
    """Convert a string of space-separated numbers into a tuple of integers."""
    return tuple(map(int, line.strip().split()))


def matches(list_1: Iterable[Any], list_2: Iterable[Any]) -> Tuple[Any, ...]:
    """Find the intersection of two lists and return it as a tuple."""
    return tuple(item for item in list_1 if item in list_2)


def process_input(data: List[str]) -> Tuple[Tuple[int, ...], ...]:
    """Processes the input data before passing it to the solution functions."""
    cards = []
    for line in data:
        winning_numbers, scratch_numbers = map(
            read_number_list, line.split(":")[1].split("|")
        )
        cards.append(matches(winning_numbers, scratch_numbers))
    return tuple(cards)


def card_points(card: Tuple[int, ...]) -> int:
    """
    Calculates the point value of a scratch card based on the number of matches.
    """
    num_matches = len(card)
    return 2 ** (num_matches - 1) if num_matches else 0


def solve_part_one(cards: Tuple[Tuple[int, ...], ...]) -> int:
    """Solves Part One of the day's challenge."""
    return sum(card_points(card) for card in cards)


def card_value_creator(all_cards: Tuple[Tuple[int, ...], ...]):
    """
    Creates a cached function to calculate the value of a card.
    """

    @lru_cache(maxsize=None)
    def card_value(index: int) -> int:
        """
        Dynamically calculate the number of cards won from a specific card
        """
        num_matches = len(all_cards[index])
        if num_matches == 0:
            return 1
        return sum(card_value(index + i) for i in range(1, num_matches + 1)) + 1

    return card_value


def solve_part_two(cards: Tuple[Tuple[int, ...], ...]) -> int:
    """Solves Part Two of the day's challenge."""
    card_value_func = card_value_creator(cards)
    return sum(card_value_func(index) for index, _ in enumerate(cards))


def main() -> None:
    input_data = read_input("input.txt")
    processed_data = process_input(input_data)

    part_one_solution = solve_part_one(processed_data)
    print(f"Part One Solution:\n{part_one_solution}")

    part_two_solution = solve_part_two(processed_data)
    print(f"Part Two Solution:\n{part_two_solution}")


if __name__ == "__main__":
    main()
