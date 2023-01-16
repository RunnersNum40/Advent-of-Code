# day9.py

from typing import NoReturn

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.strip() for line in file]


class Register:
    def __init__(self) -> NoReturn:
        self.cycles = [1]

    def signal_strength(self, cycle: int) -> int:
        """Return the signal strength of the register at a specific cycle.

        Args:
            cycle (int): Cycle to measure the strength at
        
        Returns:
            int: Signal strength
        """
        # Subtract 1 in the index because the numbers are 1-indexed
        return self.cycles[cycle-1]*cycle

    def loop(self, stream: list[str]) -> NoReturn:
        """Cycle the register over a series of commands.

        Args:
            stream (list[str]): Series of string commands
        """
        for n, line in enumerate(stream):
            self.command(line)

    def command(self, line: str) -> NoReturn:
        """Run the register on a command.
        
        Args:
            line (str): String command
        """
        if line == "noop":
            self.noop(line)
        elif line[:4] == "addx":
            self.addx(line)

    def noop(self, line: str) -> NoReturn:
        """Move forward one cycle without changing the register"""
        self.cycles.append(self.cycles[-1])

    def addx(self, line: str) -> NoReturn:
        """Add the value x after two cycles"""
        x = int(line[5:])
        self.cycles.append(self.cycles[-1])
        self.cycles.append(self.cycles[-1]+x)

    def render(self, screen_length: int = 40) -> str:
        """Render the screen given a screen length.
        
        Render each row by checking if the difference in the pixel 
        position and register differ by more than one.
        
        Args:
            screen_length (int): Length of each row in the screen
        
        Returns:
            str: String of the screen with new-lines
        """
        # Intialize a screen list (ends up 2D)
        screen = []
        for row_n in range(len(self.cycles)//screen_length):
            # Reset the row
            row = list()
            for pixel in range(screen_length):
                # Find the matching value of the register
                reg = self.cycles[row_n*screen_length+pixel]
                # Find if the register value matches the pixel position
                row.append("#" if -1 <= reg-pixel <= 1 else ".")
            screen.append("".join(row))

        return "\n".join(screen)


r = Register()
r.loop(data)

print(sum(r.signal_strength(n) for n in [20, 60, 100, 140, 180, 220]))
print(r.render())
