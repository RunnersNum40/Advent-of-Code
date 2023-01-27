# day9.py

import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.strip().split() for line in file]

data = [(direction, int(num)) for direction, num in data]


class Knot:
    # Vectors matching letters
    direction_vectors = {"R": (0, 1), "L": (0, -1), "U": (1, 0), "D": (-1, 0)}

    def __init__(self, child=None, pos: tuple[int, int] = (0, 0)):
        self.pos = np.array(pos)
        self.child = child

    def move(self, direction: str) -> None:
        """Move the knot in a direction and move it's children.

        Move in the direction then propogate the move throught the rope.

        Args:
            direction (str): R, L, U, or D directions
        """
        # Find the direction vector that matches the direction letter
        direction = self.direction_vectors[direction]
        displacement = np.array(direction)
        self.pos += displacement
        # Move children
        if self.child is not None:
            self.child.follow(self)

    def follow(self, parent) -> None:
        """Follow a parent if it's moved

        Args:
            parent (Knot): A parent knot
        """
        rel = parent.pos-self.pos
        if np.max(np.abs(rel)) > 1:
            self.pos += np.sign(rel)
        # Move children
        if self.child is not None:
            self.child.follow(self)


def count_positions(motions: list[tuple[str, int]], n_knots: int) -> int:
    """Return the number of positions visited by the tail in a series of motions.

    Move the head and tail and record the unique location

    Args:
        motions (list[tuple[str, int]]): Series of directions and lengths
        n_knots (int): Number of knots in the rope

    Returns:
        int: Number of positions visted by the tail at least once
    """
    # Create a rope at the origin
    rope = [Knot()]
    for _ in range(n_knots-1):
        rope.append(Knot(rope[-1]))
    # Create a set to record unique locations
    positions = {tuple(rope[0].pos)}
    # Track the movements
    for direction, num in motions:
        for _ in range(num):
            rope[-1].move(direction)
            positions.add(tuple(rope[0].pos))
    return len(positions)


print(count_positions(data, 2))
print(count_positions(data, 10))
