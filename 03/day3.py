# day3.py
import string

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.strip() for line in file]

letters = {letter: num for num, letter in enumerate(string.ascii_letters)}


def priority_sum(rucksacks: list) -> int:
    """Return the sum of the priorities of common elements in rucksack compartments.
    
    Split the rucksacks into compartments, find the common element in each sack and sum the priorities.
    
    Args:
        rucksacks (list): list of string rucksacks
    Returns:
        Integer sum of priorities of common elements in each sack
    """
    _priority_sum = 0
    for rucksack in data:
        compartments = rucksack[:len(rucksack)//2], rucksack[len(rucksack)//2:]
        common_elements = [item for item in compartments[0] if item in compartments[1]]
        _priority_sum += letters[common_elements[0]]+1
    return _priority_sum


def badge_priority_sum(rucksacks: list) -> int:
    """Return the sum of the priorities of common elements in groups of three.

    Split the data into groups of three, find the common element, sum the priorities.

    Args:
        rucksacks (list): list of string rucksacks
    Returns:
        Integer sum of priorities of common elements in each group of three
    """
    groups = [rucksacks[i-3:i] for i in range(3, len(rucksacks)+1, 3)]
    _priority_sum = 0
    for group in groups:
        common_elements = [item for item in group[0] if item in group[1] and item in group[2]]
        _priority_sum += letters[common_elements[0]]+1
    return _priority_sum


print(priority_sum(data))
print(badge_priority_sum(data))
