import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = np.array([int(line) for line in file])

# Part 1
def part1(data):
    # Sum the number of days greater than the previous by taking the first difference across the array
    return sum(np.diff(data)>0)

# Part 2
def part2(data):
    # Convolve across a window of [1, 1, 1] and then do same as part 1
    return part1(np.convolve(data, [1, 1, 1], "valid"))

#Running
print("Part 1:", part1(data)) # 1791
print("Part 2:", part2(data)) # 1822