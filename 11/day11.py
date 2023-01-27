# day11.py

from typing import Callable
import numpy as np
import os

# Get the current working directory
cwd = os.getcwd()
# Get the input data
file_name = "input.txt"
with open(f"{cwd}/{file_name}", "r") as file:
    data = [line.strip() for line in file]


class Monkey:
    """A monkey that inspects items and throws them to other monkeys.

    Attributes:
        number (int):
            The number of the monkey.
        items (np.ndarray):
            The worry level of each item the monkey has.
        operation (Callable[[np.ndarray], np.ndarray]):
            The operation to perform on the items when inspecting.
        test (Callable[[np.ndarray], np.ndarray]):
            The test to decide which monkey to throw an item to.
        true_monkey (int):
            The number of the monkey to throw to if the test is true.
        false_monkey (int):
            The number of the monkey to throw to if the test is false.
        inspections (int):
            The number of inspections the monkey has performed.
        """
    def __init__(self, number: int,
                 starting_items: np.ndarray,
                 operation: Callable[[np.ndarray], np.ndarray],
                 test_value: int,
                 true_monkey: int,
                 false_monkey: int) -> None:
        # Initialise the attributes
        self.number = number
        self.items = np.array(starting_items)
        self.operation = operation
        self.test_value = test_value
        self.true_monkey = true_monkey
        self.false_monkey = false_monkey
        # Initialise the number of inspections
        self.inspections = 0

    def turn(self, monkeys, divide: bool = False) -> None:
        """Inspect and then throw each item.

        Args:
            monkeys (dict[int, Monkey]):
                The monkeys in the round.
            divide (bool):
                Whether to divide the items by 3 after inspecting.
        """
        self.inspect(divide)
        self.throw(monkeys)

    def inspect(self, divide: bool) -> None:
        """Inspect the items and perform the operation on them.

        Args:
            items (np.ndarray):
                The worry level of each item the monkey has.
            divide (bool):
                Whether to divide the items by 3 after inspecting.
        """
        # Increment the number of inspections
        self.inspections += len(self.items)
        # Perform the operation on the items
        self.items = self.operation(self.items)
        # Divide the items by 3 if required
        if divide:
            self.items //= 3

    def test(self, items: np.ndarray) -> np.ndarray:
        """Test the items.

        Args:
            items (np.ndarray):
                The worry level of each item the monkey has.

        Returns:
            np.ndarray:
                A boolean array of whether the items pass the test.
        """
        return (items % self.test_value) == 0

    def throw(self, monkeys) -> None:
        """Throw the items to the correct monkeys.

        Args:
            monkeys (dict[int, Monkey]):
                The monkeys in the round.
        """
        # Find which items pass the test
        test_mask = self.test(self.items)
        # Split the items into those that pass and fail the test
        true_throws = self.items[test_mask]
        false_throws = self.items[np.logical_not(test_mask)]
        # Throw the items
        monkeys[self.true_monkey].items = np.append(monkeys[self.true_monkey].items, true_throws)
        monkeys[self.false_monkey].items = np.append(monkeys[self.false_monkey].items, false_throws)
        # Reset the items after throwing them all
        self.items = np.array([])

    @classmethod
    def from_string(cls, data: list[str]) -> object:
        """Return a Monkey object from a list of notes.

        Args:
            strings (list[str]): A list of strings containing the notes.

        Returns:
            Monkey: A Monkey object.
        """
        # Get the monkey number
        number = int(data[0].split(" ")[1].strip(":"))
        # Get the starting items
        starting_items = [int(item) for item in data[1].split(": ")[1].split(", ")]
        # Get the operation
        operation = cls.get_operation(data[2][19:].strip())
        # Get the test
        test_value = int(data[3][19:].strip())
        # Get the monkeys to throw to
        true_monkey = int(data[4].split("monkey")[1].strip())
        false_monkey = int(data[5].split("monkey")[1].strip())
        return cls(number, starting_items, operation, test_value, true_monkey, false_monkey)

    @staticmethod
    def get_operation(operation: str) -> Callable[[np.ndarray], np.ndarray]:
        """Return the operation to perform on the items when inspecting.

        Args:
            operation (str):
                The operation to perform.

        Returns:
            Callable[[np.ndarray], np.ndarray]:
                The operation to perform on the items when inspecting.

        Raises:
            ValueError: If the operation is invalid.
        """
        _, operator, value = operation.split(" ")
        if operator == "+":
            if value == "old":
                return lambda items: items * 2
            else:
                return lambda items: items + int(value)
        elif operator == "*":
            if value == "old":
                return lambda items: np.square(items)
            else:
                return lambda items: items * int(value)
        else:
            raise ValueError(f"Invalid operation: {operation}")

    def __repr__(self) -> str:
        return f"Monkey {self.number}:\n\tItems: {self.items}\n\tInspections: {self.inspections}\n\tIf true: throw to monkey {self.true_monkey}\n\tIf false: throw to monkey {self.false_monkey}"


class Game:
    def __init__(self, monkeys: list[Monkey]) -> None:
        self.monkeys = monkeys
        self.mod_value = np.lcm.reduce([monkey.test_value for monkey in self.monkeys])

    def play(self, turns: int, divide: bool = False) -> None:
        for i in range(turns):
            for monkey in self.monkeys:
                monkey.turn(self.monkeys, divide)
            # Each round module the worry levels by the mod_value
            for monkey in self.monkeys:
                monkey.items %= self.mod_value

    @property
    def monkey_business(self) -> np.int64:
        """Return the product of the number of inspections of the two most active monkeys."""
        inspections = [monkey.inspections for monkey in self.monkeys]
        return np.prod(np.sort(inspections)[-2:], dtype=np.int64)

    @classmethod
    def from_string(cls, data: list[str]) -> object:
        """Return a Game object from a list of notes.

        Args:
            strings (list[str]): A list of strings containing the notes.

        Returns:
            Game: A Game object.
        """
        # Split the data into the notes for each monkey
        monkey_data = [data[i:i+6] for i in range(0, len(data), 7)]
        # Initialise the monkeys
        monkeys = []
        for monkey_notes in monkey_data:
            monkey = Monkey.from_string(monkey_notes)
            monkeys.append(monkey)
        return cls(monkeys)


# Get the game
prob1 = Game.from_string(data)
prob2 = Game.from_string(data)
# Play the game
prob1.play(20, True)
prob2.play(10000, True)
# Get the product of the number of inspections of the two most active monkeys
print(f"Problem 1 monkey business: {prob1.monkey_business}")
print(f"Problem 2 monkey business: {prob2.monkey_business}")
