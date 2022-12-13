# day5.py

import re


file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line for line in file]

piles_end = data.index("\n")

# Find each of the piles by iterating down each column
piles = []
for n in range(1, len(data[0]), 4):
    # Iterate through each row in reverse order so the top of the pile is the end
    piles.append([row[n] for row in data[piles_end-2::-1] if row[n] != " "])
piles = tuple(piles)

# Find the move commands by extracting with regex
pattern = r"move (\d+) from (\d+) to (\d+)"
steps = []
for row in data[piles_end+1:]:
    match = re.search(pattern, row)
    step = tuple(map(int, (match.group(1), match.group(2), match.group(3))))
    steps.append(step)


def apply_step_reverse(piles: tuple, step: tuple) -> list:
    """Apply a move command to a list of piles with reverse order for the new pile
    
    Args:
        piles (tuple): Tuple of list of piles
        step (tuple): Tuple with (amount to move, pile to move from, pile to move to)
    Returns:
        The new configuration of piles
    """
    piles = tuple(pile.copy() for pile in piles)
    for _ in range(step[0]):
        piles[step[2]-1].append(piles[step[1]-1].pop())

    return piles


def apply_step_forward(piles: tuple, step: tuple) -> list:
    """Apply a move command to a list of piles with the same order for the new pile
    
    Args:
        piles (tuple): Tuple of list of piles
        step (tuple): Tuple with (amount to move, pile to move from, pile to move to)
    Returns:
        The new configuration of piles
    """
    piles = tuple(pile.copy() for pile in piles)
    piles[step[2]-1].extend(piles[step[1]-1][-step[0]:])
    del piles[step[1]-1][-step[0]:]

    return piles


reverse = piles
forward = piles
for n, step in enumerate(steps):
    reverse = apply_step_reverse(reverse, step)
    forward = apply_step_forward(forward, step)

print("".join(pile[-1] for pile in reverse))
print("".join(pile[-1] for pile in forward))
