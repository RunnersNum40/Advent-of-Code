import numpy as np

file_name = "input.txt"
with open(file_name, "r") as file:
    # Read each line of the input
    data = [line.strip() for line in file]
    # Take the draws and convert to ints
    draws = np.array(list(map(int, data.pop(0).split(","))))
    # Retrieve the boards
    boards = np.array([[list(map(int, row.split())) for row in data[n+1:n+6]] for n in range(0, len(data), 6)])

# Part 1
def check_row(draws, row):
    # Check that every item in a row occurs in the draws
    for item in row:
        if item not in draws:
            return False
    else:
        return True

def check_board(draws, board):
    # Check horizontally
    for row in board:
        if check_row(draws, row):
            return True
    # Check vertically
    for col in board.T:
        if check_row(draws, col):
            return True
    # Not complete
    return False

def score_board(draws, board):
    # Replace every number drawn with 0
    for n in draws:
        board[board==n] = 0
    # Sum the undrawn numbers
    return np.sum(board)

def part1(draws, boards):
    # Copy the draws and boards so the originals are not overwritten
    draws = np.copy(draws)
    boards = np.copy(boards)
    # Loop through all the draws in order
    for n in range(len(draws)):
        # Check for a winning board
        for board in boards:
            # Check if the board wins in this turn
            if check_board(draws[:n+1], board):
                return score_board(draws[:n+1], board)*draws[n]


# Part 2
def part2(draws, boards):
    # Copy the draws and boards so the originals are not overwritten
    draws = np.copy(draws)
    boards = np.copy(boards)
    # Loop through all the draws in order
    for n in range(len(draws)):
        # Check for a winning board
        remove = []
        # Log any boards that win
        for i, board in enumerate(boards):
            if check_board(draws[:n+1], board):
                remove.append(i)
        # Remove any boards that win
        for i in remove[::-1]:
            boards = np.delete(boards, i, 0)
        # Stop when there is one board left
        if boards.shape[0] == 1:
            break
    # Loop until the last board wins
    for n in range(len(draws)):
        # Log any boards that win
        if check_board(draws[:n+1], board):
            # Score the final win
            return score_board(draws[:n+1], boards[0])*draws[n]

#Running
print("Part 1:", part1(draws, boards)) # 11536
print("Part 2:", part2(draws, boards)) # 1284