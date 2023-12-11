import numpy as np
from scipy.stats import mode

file_name = "input.txt"
with open(file_name, "r") as file:
    # Convert to a 2d array 
    data = np.array([list(map(int, line.strip())) for line in file])

def decimal(arr, carry=0):
    # Take a binary number in list form and reduce to an integer
    l = len(arr)
    if l==1:
        # Escape condition (2**0=1)
        return carry+arr[0]
    else:
        # Multiply the left most digit and repeat for remainaing
        return carry+arr[0]*2**(l-1)+decimal(arr[1:], carry)

# Part 1
def part1(data):
    # Take the mode of each column
    gamma = np.array(mode(data)[0][0])
    # Reverse each item 0->1, 1->0
    epsilon = 1-gamma
    # Convert to decimal and multiply
    return decimal(gamma)*decimal(epsilon)

# Part 2
def part2(data):
    # Find the most common bits in each column
    most_common = np.array(mode(data)[0][0])
    # Copy data to two new arrays
    o2, co2, = map(np.copy, (data, data))
    # For each column
    for n in range(data.shape[1]):
        # Filter out lists that do not have the correct value for that column
        o2 = o2[o2[:,n]==mode(list(o2[:,n])+[1])[0][0]] if o2.shape[0]>1 else o2
        co2 = co2[co2[:,n]!=mode(list(co2[:,n])+[1])[0][0]] if co2.shape[0]>1 else co2
    # Convert the remaining rows to decimal and multiply
    return decimal(o2[0])*decimal(co2[0])


#Running
print("Part 1:", part1(data)) # 4001724
print("Part 2:", part2(data)) # 587895