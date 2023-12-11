import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = np.array([list(map(int, line.strip().split(","))) for line in file][0])

# Part 1
def part1(data):
    # Return the minimum of the prices for each possible position
    return min(np.sum(np.abs(data-n)) for n in range(int(np.min(data)), int(np.max(data))))

# Part 2
def part2(data):
    # List to store the fuel costs
    fuel = []
    # Store the triangle number n(n+1)/2 of the horizontal distances
    for n in range(int(np.min(data)), int(np.max(data))):
        # Take the horizontal distances
        diff = np.abs(data-n)
        # Triangle number and the square the distances
        fuel.append(np.sum((diff*(diff+1))/2))
    # Return the minimum cost
    return min(fuel)

#Running
print("Part 1:", part1(data)) # 344138
print("Part 2:", part2(data)) # 94862124