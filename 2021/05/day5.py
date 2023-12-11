import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    data = np.array([[list(map(int, point.split(","))) for point in line.split(" -> ")] for line in file])

def draw_lines(lines):
    grid = np.zeros((max(max(lines[:,0,1]), max(lines[:,1,1]))+1, max(max(lines[:,0,0]), max(lines[:,1,0]))+1))
    # Add the lines to the grid
    for line in lines:
        # Create the line trajectory
        p1, p2 = line[0], line[1]
        step = p2-p1
        # Scale the trajectory to a step size of one
        n = max(step)
        if n > 0:
            step = step/n
        else:
            step = -step
            n = max(step)
            step = step/n
            p1, p2 = p2, p1

        # Iterate over the line
        for i in range(n+1):
            x, y = map(int, p1+step*i)
            # Add 1 to every point along the line
            grid[y][x] += 1

    return grid

# Part 1
def part1(data):
    # Copy data so the oringinal is not overwritten
    data = np.copy(data)
    # Filter out lines that are not horizontal or vertical
    data = data[np.logical_or(data[:,0,0]==data[:,1,0], data[:,0,1]==data[:,1,1])]
    # Create the grid
    grid = draw_lines(data)
    # Check for any points with more than one line
    return np.count_nonzero(grid >= 2)

# Part 2
def part2(data):
    # Copy data so the oringinal is not overwritten
    data = np.copy(data)
    # Create the grid
    grid = draw_lines(data)
    # Check for any points with more than one line
    return np.count_nonzero(grid >= 2)

#Running
print("Part 1:", part1(data)) # 8350
print("Part 2:", part2(data)) # 19374