# day4.py

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line for line in file]

pairs = [[list(map(int, elf.split("-"))) for elf in pair.split(",")] for pair in data]


def self_containing(pair: list) -> bool:
    """Return if one assignment in a pair fully contains the other.

    Check if the first contains the second, then check the sencond on the first.

    Args:
        pair (list): List of assignments
    Returns:
        True if one contains the other else False
    """
    # The first assignment contains the second
    if pair[0][0] <= pair[1][0] and pair[0][1] >= pair[1][1]:
        return True
    # The second assignment contains the first
    elif pair[0][0] >= pair[1][0] and pair[0][1] <= pair[1][1]:
        return True
    # Neither contains the other
    else:
        return False


def self_overlapping(pair: list) -> bool:
    """Return if the assignments overlap each other.

    Check if one pair contains the other then, 
    check if either of the ends of assignment one are within assignment two.

    Args:
        pair (list): List of assignments
    Returns:
        True if the assignments overlap each other else False
    """
    if self_containing(pair):
        return True
    # The begining of the assignment one is with assignment two.
    elif pair[1][0] <= pair[0][0] <= pair[1][1]:
        return True
    # The end of the assignment one is with assignment two.
    elif pair[1][0] <= pair[0][1] <= pair[1][1]:
        return True
    # There is no overlap
    else:
        return False


print(sum(map(self_containing, pairs)))
print(sum(map(self_overlapping, pairs)))
