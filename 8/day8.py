# day8.py

import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.strip() for line in file]

data = np.array([list(map(int, line)) for line in data])
print(data)


def invisible(line: list, tree: int) -> bool:
    """Return if the tree is invisible from the edge.
    
    Check if any of the trees are >= to the original tree.
    
    Args:
        line (list): height of trees
        tree (int): height of the tree to compare to
    
    Returns:
        bool: False if the tree is visible from the edge else True.
    """
    return any(j >= tree for j in line)


def visible(grid: list, pos: tuple) -> bool:
    """Return if the tree at a position in the grid is visible.
    
    Check if the tree is not the smallest in the up, 
    down, right, and left directions.
    
    Args:
        grid (list): np Array of ints
        pos (tuple): index of tree to check

    Returns:
        bool: True if the tree is visible
    """
    for n, i in enumerate(pos):
        # Generate a slice along one axis
        index = tuple([pos[j] if j != n else ... for j in range(len(pos))])
        line = grid[index]
        # Get both directions
        dir1 = invisible(line[:i], grid[pos])
        dir2 = invisible(line[i+1:], grid[pos])
        # Find the visibility in the positive and negative directions
        if not (dir1 and dir2):
            return True
    return False


def view(line: list, tree: int) -> int:
    """Return the number of trees less than a value starting from the beginning
    
    Loop until a tree too tall is found or there are no trees.
    
    Args:
        line (list): height of trees getting farther from the start
        tree (int): height of the tree to compare to
    
    Returns:
        int: Number of visible trees
    """
    for n, i in enumerate(line):
        if i >= tree:
            return n+1
    return len(line)


def scenic_score(grid: list, pos: tuple) -> int:
    """Return the scenic score of a tree in the grid.
    
    Return the product of the number of visible trees in each direction.
    
    Args:
        grid (list): np Array of ints
        pos (tuple): index of tree to check
    
    Returns:
        int: The product of the number of visible trees
    """
    dirs = []
    for n, i in enumerate(pos):
        # Generate a slice along one axis
        index = tuple([pos[j] if j != n else ... for j in range(len(pos))])
        line = grid[index]
        # Find the scenic value in the positive and negative directions
        dirs.append(view(line[:i][::-1], grid[pos]))
        dirs.append(view(line[i+1:], grid[pos]))
    return np.prod(dirs)


print(sum(visible(data, ix) for ix in np.ndindex(*data.shape)))
print(max(scenic_score(data, ix) for ix in np.ndindex(*data.shape)))
