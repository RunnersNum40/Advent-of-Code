# day12.py

from typing import Callable
import numpy as np
import string
import os

from graph import Graph

# Get the current working directory
cwd = os.getcwd()
# Get the input data
file_name = "input.txt"
with open(f"{cwd}/{file_name}", "r") as file:
    data = [list(line.strip()) for line in file]

class HeightMap:
    """A class to represent a height map and support searching

    Attributes:
        height_map (np.ndarray): A numpy array representing the height map
        start (tuple[int, int]): The start position
        objective (tuple[int, int]): The objective position
    """
    def __init__(self, height_map: np.ndarray, start: tuple[int, int], objective: tuple[int, int]):
        self.height_map = height_map
        self.start = start
        self.objective = objective

    @classmethod
    def from_string(cls, data: list[list[str]]) -> object:
        """Return a Map object from a list of strings

        Args:
            string (list[list[str]]): List of strings

        Returns:
            Map: A Map object
        """
        # Find the start position and objective ('S' and 'E' respectively)
        for x, line in enumerate(data):
            for y, char in enumerate(line):
                if char == "S":
                    start = (x, y)
                elif char == "E":
                    objective = (x, y)
        # Convert the data to a numpy array
        num_map = {letter: num for num, letter in enumerate(string.ascii_lowercase)}
        height_map = np.array([[num_map[char.lower()] for char in line] for line in data])
        height_map[start] = 0
        height_map[objective] = 25
        return cls(height_map, start, objective)

    def __repr__(self) -> str:
        """Return a string representation of the height map"""
        return str(self.height_map)


def huristic(node: object, goal: object) -> int:
    """Return the manhattan distance between the node and the objective.

    Args:
        node (object): The node to evaluate
        goal (tuple[int, int]): The objective position

    Returns:
        int: The huristic value
    """
    return abs(node.position[0] - goal.position[0]) + abs(node.position[1] - goal.position[1])


# Create a HeightMap object
height_map = HeightMap.from_string(data)
# Create a Graph object from the height map
graph = Graph.from_height_map(height_map.height_map)
# Find the shortest path
# Find the start node
start_node = graph.nodes[height_map.start[0]][height_map.start[1]]
# Find the objective node
objective_node = graph.nodes[height_map.objective[0]][height_map.objective[1]]
# Find the shortest path
path = graph.search(start_node, objective_node, huristic)
# Print the path
print(len(path)-1)

# Find all the nodes at height 0
start_nodes = [node for row in graph.nodes for node in row if node.height == 0]
# Find the shortest path to the objective from each start node
paths = []
for node in start_nodes:
    try:
        path = graph.search(node, objective_node, huristic)
    except ValueError:
        continue
    paths.append(path)

# Print the shortest path
print(len(min(paths, key=len))-1)