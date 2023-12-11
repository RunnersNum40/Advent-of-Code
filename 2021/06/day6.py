import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    # Read the first line into an np array of ints
    data = np.array(list(map(int, file.readline().split(","))))

# This is a transformation matrix that takes day(n) to day(n+1)
# boost*[x0, x1, x2, x3, x4, x5, x6, x7, x8] -> [x1, x2, x3, x4, x5, x6, x7+x0, x8, x0]
boost = np.array([[0, 1, 0, 0, 0, 0, 0, 0, 0],
                  [0, 0, 1, 0, 0, 0, 0, 0, 0],
                  [0, 0, 0, 1, 0, 0, 0, 0, 0],
                  [0, 0, 0, 0, 1, 0, 0, 0, 0],
                  [0, 0, 0, 0, 0, 1, 0, 0, 0],
                  [0, 0, 0, 0, 0, 0, 1, 0, 0],
                  [1, 0, 0, 0, 0, 0, 0, 1, 0],
                  [0, 0, 0, 0, 0, 0, 0, 0, 1],
                  [1, 0, 0, 0, 0, 0, 0, 0, 0]])

def fishes(data, day=80):
    # Convert the fishes into a list of number of timers fore each value
    data = np.array([np.count_nonzero(data==n) for n in range(9)])
    # Take the power of the boost matrix to preform many days of changes at once
    transform = np.linalg.matrix_power(boost, day)
    # Boost the intital conditions
    data = np.matmul(transform, data, dtype=np.int64)
    # Return the number of fish
    return np.sum(data)

#Running
print("Part 1:", fishes(data, 80)) # 371379
print("Part 2:", fishes(data, 256)) # 1674303997472