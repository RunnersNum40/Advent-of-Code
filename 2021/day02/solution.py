import numpy as np


def parse_line(command, number):
    return (command, int(number))


file_name = "input.txt"
with open(file_name, "r") as file:
    data = [parse_line(*line.split(" ")) for line in file]


def part1(data):
    lookup = {
        "forward": np.array([1, 0]),
        "up": np.array([0, -1]),
        "down": np.array([0, 1]),
    }
    pos = np.array([0, 0])

    for command, number in data:
        # Lookup the number mask and add the scaled mask to pos
        pos += lookup[command] * number

    return pos[0] * pos[1]


def part2(data):
    mask = np.array([1, 0])
    pos = np.array([0, 0])

    for command, number in data:
        if command == "forward":
            # Add to position
            pos += mask * number
        else:
            # Increase or decrease aim
            mask[1] += {"up": -1, "down": 1}[command] * number

    return pos[0] * pos[1]


# Running
print("Part 1:", part1(data))  # 1936494
print("Part 2:", part2(data))  # 1997106066
