import numpy as np

file_name = "test.txt"
with open(file_name, "r") as file:
    patterns = []
    values = []
    # Take a char and return the segment number
    rep = lambda l: list("abcdefg").index(l)
    # Take a string and return the segment numbers
    num = lambda string: list(map(rep, string))
    # For every line in the file
    for line in file:
        # Split the line across the delimiter
        p, v = line.strip().split("|")
        # Split both sections into digits
        p, v = p.strip().split(), v.strip().split()
        # Turn each char into a segment number
        p, v = list(map(num, p)), list(map(num, v))
        # Turn the segment numbers into signals
        p = [[1 if i in psub else 0 for i in range(7)] for psub in p]
        v = [[1 if i in vsub else 0 for i in range(7)] for vsub in v]
        # Store the current line
        patterns.append(p); values.append(v)
    # Convert the 3D list into an np array
    patterns = np.array(patterns); values = np.array(values)

def swap3d(arr, start_index, last_index, axis=0):
    """Take a 3D np array and swap the two specified indexes along the axis"""
    arr = np.copy(arr)
    # Swap rows
    if axis == 0:
        arr[[start_index, last_index]] = arr[[last_index, start_index]]
    # Swap cols
    elif axis == 1:
        arr[:, [start_index, last_index]] = arr[:, [last_index, start_index]]
    # Don't deal with more axies
    else:
        raise ValueError(f"Axis = {axis} must be 0 or 1")
    return arr

def decode(pattern, values, v=False):
    """Take a set of ten digits and a set of values to decode. 
       Rearrange the cols until all are in the correct place"""
    if v: print(pattern, end="p\n\n")
    # The [rows, col] in the correct position
    confirmed = [set(), set()]
    # The (number of on segments, digit displayed) for all the digit with a unique number of segments
    unique_rows = {2:1, 4:4, 3:7, 7:8}
    for i in range(pattern.shape[0]):
        for _ in range(2):
            # Count the number of segments on in this digit
            segments = np.sum(pattern[i])
            # If the number of segments is unique
            if i not in confirmed[0] and segments in unique_rows.keys():
                # Swap the row into the correct place
                if v: print(f"Swapping rows {i} and {unique_rows[segments]}, because {i} has {segments} segments in it")
                pattern = swap3d(pattern, i, unique_rows[segments], 0)
                # Log the correct rows
                confirmed[0].add(unique_rows[segments])
    if v: print(confirmed)
    if v: print(pattern, end="p\n\n")
    if v: print(values, end="v\n\n")
    # The (number of times used, col) for all the segments with a unique number of uses
    unique_cols = {6:1, 4:4, 9:5}
    for i in range(pattern.shape[1]):
        for _ in range(2):
            # Count the number of uses of this segment
            uses = np.sum(pattern[:,i])
            # If the number of uses is unique
            if i not in confirmed[1] and uses in unique_cols.keys():
                # Swap the col into the correct place
                if v: print(f"Swapping cols {i} and {unique_cols[uses]}, because {i} has {uses} segments used")
                pattern = swap3d(pattern, i, unique_cols[uses], 1)
                # Swap the corresponding cols in value
                values = swap3d(values, i, unique_cols[uses], 1)
                # Log the correct cols
                confirmed[1].add(unique_cols[uses])
    if v: print(confirmed)
    if v: print(pattern, end="p\n\n")
    if v: print(values, end="v\n\n")
    # The unconfirmed col of row with a one is the coln col
    for row, coln in ((1, 2), (7, 0), (4, 3)):
        # The unconfirmed col of row one that has a one is col two (letter c)
        for i in range(7):
            # Find the segment that is one
            if i not in confirmed[1] and pattern[row][i] == 1:
                if v: print(f"Swapping cols {i} and {coln}, because {i} has is a 1 in an unconfirmed segment of {row}")
                # Swap the col into the correct place
                pattern = swap3d(pattern, i, coln, 1)
                values = swap3d(values, i, coln, 1)
                # Log the correct cols
                confirmed[1].add(coln)
        if v: print(confirmed)
        if v: print(pattern, end="p\n\n")
        if v: print(values, end="v\n\n")
    # Return the reordered inputs
    return values

def read_segments(digit):
    print(digit)
    # Each digit's segment setup
    key = [[1, 1, 1, 0, 1, 1, 1, 0],
           [0, 0, 1, 0, 0, 1, 0, 1],
           [1, 0, 1, 1, 1, 0, 1, 2],
           [1, 0, 1, 1, 0, 1, 1, 3],
           [0, 1, 1, 1, 0, 1, 0, 4],
           [1, 1, 1, 1, 0, 1, 1, 5],
           [1, 1, 0, 1, 1, 1, 1, 6],
           [1, 0, 1, 0, 0, 1, 0, 7],
           [1, 1, 1, 1, 1, 1, 1, 8],
           [1, 1, 0, 1, 0, 1, 1, 9]]
    # For each digit
    for x in range(7):
        print(x)
        # For each segment
        for y in range(len(key), 0, -1):
            print(key[y-1], key[y-1][x], digit[x])
            # If the segments to not match
            if key[y-1][x] != digit[x]:
                # Discard the digit
                del key[y-1]
    # Return the remaining digit
    return key[0][-1]

# Part 1
def part1(patterns, values):
    # Sum variable
    s = 0
    # For each set of outputs
    for value in values:
        # Sum the number of digits with a unique number of segments
        s += sum(np.sum(value[i]) in (2, 4, 3, 7) for i in range(value.shape[0]))
    return s

# Part 2
def part2(patterns, values):
    # Store the sum digits
    s = 0
    # For each set of data
    for pattern, value in zip(patterns, values):
        # Decode the data into the correct order
        decoded = decode(pattern, value, True)
        # Find the value of each digit
        decoded = sum(read_segments(digit)*10**(len(decoded)-n-1) for n, digit in enumerate(decoded))
        s += decoded
    # Sum the digits
    return s

#Running
print("Part 1:", part1(patterns, values)) # 445
print("Part 2:", part2(patterns, values)) # 