# day12.py

from typing import Union
import os

# Get the current working directory
cwd = os.getcwd()
# Get the input data
file_name = "input.txt"
with open(f"{cwd}/{file_name}", "r") as file:
    data = [line.strip() for line in file]


def generate_pairs(data: list[str]) -> list[tuple[Union[int, list], Union[int, list]]]:
    """Generates a list of pairs from the input data.

    Each element of a pair is a list of numbers and lists.

    Args:
        data (list[str]): The input data as strings

    Returns:
        list[tuple[Union[int, list], Union[int, list]]]: A list of tuples of pairs
    """
    pairs = []
    for i in range(0, len(data), 3):
        # I know eval() is unsecure but I'm not using it on user input and it's easier than writing a parser
        # + it's my computer so I can do what I want
        pairs.append((eval(data[i]), eval(data[i + 1])))
    return pairs


def check_order(pair: tuple[Union[int, list], Union[int, list]], indent: int = 0) -> bool:
    """Checks if the order of the pair is correct.

    To compare lists, compare matching values in the lists.
    If both values are integers, the lower integer should come first.
    eg. (1, 2) is correct, (2, 1) is not.

    If both values are compare the each value of each list. If a list runs out first then the other list is greater.
    eg. ([1, 2], [1, 2, 3]) is correct, ([1, 2, 3], [1, 2]) is not.

    If one value is a list and the other is an integer, convert the integer to a list of the same length.
    eg. ([1, 2], 1) becomes ([1, 2], [1, 1]).

    Args:
        pair (tuple[Union[int, list], Union[int, list]]): A pair of numbers or lists

    Returns:
        bool: True if the order is correct, False otherwise, None if the order cannot be determined
    """
    indent_string = "  "*indent
    print(f" {indent_string}- Compare {pair[0]} vs {pair[1]}")
    if isinstance(pair[0], int):
        # Both are ints
        if isinstance(pair[1], int):
            # Check if the first value is less than the second
            if pair[0] < pair[1]:
                return True
            elif pair[0] > pair[1]:
                return False
            else:
                return None
        # The first value is an int and the second is a list
        else:
            # Convert the first value to a list
            new_pair = ([pair[0] for _ in range(len(pair[1]))], pair[1])
            # Check the order of the new pair
            return check_order(new_pair, indent + 1)
    else:
        # The first value is a list and the second is an int
        if isinstance(pair[1], int):
            # Convert the second value to a list
            new_pair = (pair[0], [pair[1] for _ in range(len(pair[0]))])
            # Check the order of the new pair
            return check_order(new_pair, indent + 1)
        # Both are lists
        else:
            for x, y in zip(pair[0], pair[1]):
                # Check if the first value is less than the second
                order = check_order((x, y), indent + 1)
                if order is not None:
                    return order
            if len(pair[0]) < len(pair[1]):
                return True
            elif len(pair[0]) > len(pair[1]):
                return False
            else:
                return None


def find_pairs(pairs: list[tuple[Union[int, list], Union[int, list]]]) -> list[int]:
    """Find the pairs in the correct order.

    Args:
        pairs (list[tuple[list]]): A list of pairs

    Returns:
        list[int]: A list of the indexes of the pairs in the correct order
    """
    return [i+1 for i, pair in enumerate(pairs) if check_order(pair)]


# Generate the pairs
pairs = generate_pairs(data)
for i, pair in enumerate(pairs):
    print(i+1, check_order(pair))
# Find the first pair that is in the correct order
correct_pairs = find_pairs(pairs)
print(correct_pairs)
# Print the sum of the indexes of the pairs
print(sum(correct_pairs))
