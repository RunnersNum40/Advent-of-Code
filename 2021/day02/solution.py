import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    process = lambda command, number: (command, int(number))
    data = [process(*line.split(" ")) for line in file]

# Part 1
def part1(data):
    lookup = {"forward":np.array([1, 0]), "up":np.array([0, -1]), "down":np.array([0, 1])} # Lookup table for masks
    pos = np.array([0, 0]) # Initial position [x, y]

    for command, number in data:
        # Lookup the number mask and add the scaled mask to pos
        pos += lookup[command]*number

    return pos[0]*pos[1]

# Part 2
def part2(data):
    mask = np.array([1, 0]) # Intialize the mask, [1, aim]
    pos = np.array([0, 0]) # Initial position, [x, y]

    for command, number in data:
        if command == "forward":
            # Add to position
            pos += mask*number
        else:
            # Increase or decrease aim
            mask[1] += {"up":-1, "down":1}[command]*number

    return pos[0]*pos[1]

#Running
print("Part 1:", part1(data)) # 1936494
print("Part 2:", part2(data)) # 1997106066