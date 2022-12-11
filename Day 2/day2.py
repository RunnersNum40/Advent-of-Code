# day2.py

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.split() for line in file]


def score_hand(player1: str, player2: str) -> int:
    """Return the score that player2 recives for the round.

    Args:
        player1: A play out of "A", "B", "C" (rock, paper, scissors)
        player2: A play out of "X", "Y", "Z" (rock, paper, scissors)
    Returns:
        The score of player2 in the round given.
    """
    # Determine the score provided for given plays
    score_matrix = ((3, 6, 0), (0, 3, 6), (6, 0, 3))
    # Determine the score-1 that a play is worth
    player1 = {"A": 0, "B": 1, "C": 2}[player1.upper()]
    player2 = {"X": 0, "Y": 1, "Z": 2}[player2.upper()]
    return score_matrix[player1][player2]+player2+1

def required_throw(player1, result):
    """Return the score earned by player2 for the result given with player1's play.

    Args:
        player1: A play out of "A", "B", "C" (rock, paper, scissors)
        result: A result out of "X", "Y", "Z" (lose, draw, win)
    Returns:
        The score of player2 in the round given.
    """
    player1 = {"A": 0, "B": 1, "C": 2}[player1.upper()]
    result = {"X": 0, "Y": 1, "Z": 2}[result.upper()]

    return (3, 1, 2)[(player1+result)%3]+result*3



print(sum([score_hand(player1, player2) for player1, player2 in data]))
print(sum([required_throw(player1, result) for player1, result in data]))
