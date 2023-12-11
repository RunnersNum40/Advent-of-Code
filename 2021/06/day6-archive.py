import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = np.array([list(map(int, line.split(","))) for line in file][0])

def fishes(n, days, seen={}, calls=[0]):
    """ Take a fish's days until creating a new fish and the number of days left.
        Return the number of fish after that many days."""

    # Check if the problem has already been solved
    if (n, days) in seen.keys():
        pass
    # If there are not enough days left for a new fish
    elif n >= days or days == 0:
        # Only the current fish exists
        seen[(n, days)] = 1
    # If there are exactly enough days to get one new fish
    elif n == days+1:
        # Two fish exist at the end
        seen[(n, days)] = 2
    # If there are enough days to spawn new fish with time left over
    else:
        # The current fish continues and it's spawn
        seen[(n, days)] = fishes(7, days-n)+fishes(9, days-n)
    # Return the number of resulting fish
    return seen[(n, days)]

# Part 1
def part1(data):
    s = 0
    for n in data:
        s += fishes(n, 80)
    return s

# Part 2
def part2(data):
    s = 0
    for n in data:
        s += fishes(n, 256)
    return s

#Running
print("Part 1:", part1(data)) # 371379
print("Part 2:", part2(data)) # 1674303997472