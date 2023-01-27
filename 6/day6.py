# day6.py

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line for line in file]


def check_no_repeats(signal: str) -> bool:
    """Check if a string has no repeated characters.
    
    Convert to a set and check that the length stays the same.
    
    Args:
        signal (str): Run of characters
    Returns:
        True if there are no repeated chars else False
    """
    return len(signal) == len(set(signal))


def no_repeat_run(signal: str, match_length: int) -> int:
    """Return the number of characters in the signal till there are no repeats in a run.
    
    Args:
        signal (str): Sring of the signal
        match_length (int): Length of the run to check
    Returns:
        int: Number of characters till no repeats in a run
    """
    for n in range(len(signal)-match_length):
        if check_no_repeats(signal[n:n+match_length]):
            return n+match_length


print(no_repeat_run(data[0], 4))
print(no_repeat_run(data[0], 14))
