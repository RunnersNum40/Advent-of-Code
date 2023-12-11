# graph.py

from typing import Callable
import numpy as np

class Node:
    """A class to represent a node in a graph.
    
    Attributes:
        height (int): The height of the node
        position (tuple[int, int]): The position of the node
        neighbours (list[Node]): A list of the node's neighbours
    """
    def __init__(self, height: int, position: tuple[int, int]):
        self.height = height
        self.position = position
        self.neighbours = []

    def add_neighbour(self, neighbour: object) -> None:
        """Add a neighbour to the node

        Args:
            neighbour (object): The neighbour to add
        """
        self.neighbours.append(neighbour)

    def __repr__(self) -> str:
        """Return a string representation of the node"""
        return f"Node({self.height})"
    
class Graph:
    """A class to represent a graph. Each node has a height and a list of neighbours.
    
    Attributes:
        nodes (list[Node]): A list of the nodes in the graph
    """
    def __init__(self, nodes: list[Node]):
        self.nodes = nodes

    @classmethod
    def from_height_map(cls, height_map: np.ndarray) -> object:
        """Return a Graph object from a height map.

        It's neighbours are the nodes next to it in the height map and <= one height unit above.

        Args:
            height_map (np.ndarray): A numpy array representing the height map

        Returns:
            Graph: A Graph object
        """
        # Create a list of nodes
        nodes = [[Node(height, (x, y)) for y, height in enumerate(row)] for x, row in enumerate(height_map)]
        # Add the neighbours
        for x, row in enumerate(nodes):
            for y, node in enumerate(row):
                # Check the node above
                if x > 0 and height_map[x-1, y] - node.height <= 1:
                    node.add_neighbour(nodes[x-1][y])
                # Check the node below
                if x < len(nodes) - 1 and height_map[x+1, y] - node.height <= 1:
                    node.add_neighbour(nodes[x+1][y])
                # Check the node to the left
                if y > 0 and height_map[x, y-1] - node.height <= 1:
                    node.add_neighbour(nodes[x][y-1])
                # Check the node to the right
                if y < len(row) - 1 and height_map[x, y+1] - node.height <= 1:
                    node.add_neighbour(nodes[x][y+1])
        return cls(nodes)

    def to_height_map(self) -> np.ndarray:
        """Return a numpy array representing the height map

        Returns:
            np.ndarray: A numpy array representing the height map
        """
        return np.array([[node.height for node in row] for row in self.nodes])

    def __repr__(self) -> str:
        """Return a string representation of the graph"""
        return str(self.to_height_map())

    @staticmethod
    def reconstruct_path(came_from: dict[object, object], current: object) -> list[object]:
        """Reconstruct the path from the start to the current node

        Args:
            came_from (dict[object, object]): A dictionary mapping each node to its parent
            current (object): The current node

        Returns:
            list[object]: A list of nodes representing the path
        """
        total_path = [current]
        while current in came_from:
            current = came_from[current]
            total_path.append(current)
        return total_path[::-1]

    @staticmethod
    def search(start_node: object, goal: object, h: Callable[[object, object], float]) -> list[object]:
        """Search the graph for the shortest path from the start to the goal

        Use the A* algorithm to find the shortest path from the start to the objective.

        Args:
            start_node (object): The start node
            goal (object): The objective node
            h (Callable[[object, object], float]): The heuristic function to use

        Returns:
            list[object]: A list of nodes representing the shortest path

        Raises:
            ValueError: If no path is found
        """
        open_set = [start_node]
        came_from = {}
        g_score = {start_node: 0}
        f_score = {start_node: h(start_node, goal)}
        while open_set:
            current = min(open_set, key=lambda node: f_score[node])
            if current == goal:
                return Graph.reconstruct_path(came_from, current)
            open_set.remove(current)
            for neighbour in current.neighbours:
                tentative_g_score = g_score[current] + 1
                if neighbour not in g_score or tentative_g_score < g_score[neighbour]:
                    came_from[neighbour] = current
                    g_score[neighbour] = tentative_g_score
                    f_score[neighbour] = tentative_g_score + h(neighbour, goal)
                    if neighbour not in open_set:
                        open_set.append(neighbour)

        raise ValueError("No path found")
