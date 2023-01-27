# day7.py

from collections.abc import Callable
import re

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line.strip() for line in file]


class Folder:
    """A folder class that contains subfolders and files"""
    pattern = r"(\d+) ([a-z]+.*[a-z]*)"

    def __init__(self, 
                 parent: object = None,
                 name: str = "/",
                 subfolders: dict = None,
                 files: dict = None):

        self.parent = parent
        self.name = name
        # Initialize a dict if none given
        self.subfolders = subfolders if subfolders is not None else {}
        self.files = files if files is not None else {}

    def execute_line(self, line):
        """Emulate the result of line occuring in the terminal
        while the folder is open.

        Check which type of line occured and run the matching function.

        Args:
            line (str): String on the line

        Returns:
            Folder: Returns the folder open after line execution

        Raises:
            ValueError: If a line is not a recognized command
        """
        if line[0] == "$":
            if line[2:4] == "cd":
                return self.cd(line)
            elif line[2:4] == "ls":
                return self.ls(line)
            else:
                raise ValueError(f"{line} command not recognized")
        else:
            return self.output(line)

    def cd(self, line: str):
        """Emulate the result of cd occuring in the terminal 
        while the folder is open.

        Create a new subfolder if it does not exist then return the folder

        Args:
            line (str): The cd command

        Returns:
            Folder Returns the folder open after line execution
        """
        folder = line[5:]
        if folder == "..":
            return self.parent
        elif folder == "/":
            if self.name == "/":
                return self
            else:
                return self.parent.cd(line)
        # Folder already exists
        if folder not in self.subfolders:
            self.subfolders[folder] = Folder(parent=self, name=folder)
        return self.subfolders[folder]

    def ls(self, line: str):
        """Emulate the result of ls occuring in the terminal 
        while the folder is open.

        Args:
            line (str): The ls command

        Returns:
            Folder Returns the folder open after line execution
        """
        return self

    def output(self, line: str):
        """Emulate the result of an output occuring in the terminal
        while the folder is open.

        Args:
            line (str): The terminal output

        Returns:
            Folder: Folder Returns the folder open after line execution
        """
        if line[:3] == "dir":
            folder = line[4:]
            if folder not in self.subfolders:
                self.subfolders[folder] = Folder(parent=self, name=folder)
        else:
            match = re.search(self.pattern, line)
            size, name = int(match.group(1)), match.group(2)
            self.files[name] = size

        return self

    def asdict(self) -> dict:
        """Return the dictionary representation of a folder.

        Create a nested dictionary of the dictionaies of subfolders.

        Returns:
            dict: A nested dictionary of folders and files
        """
        # Recursively find the dicts of subfolders
        contents = {name: folder.asdict() for name, folder in self.subfolders.items()}
        contents.update(self.files)
        return contents

    def size(self) -> int:
        """Return the size of the folder including subfolders.

        Returns:
            int: size of the folder
        """
        files_size = sum(self.files.values())
        subfolders_size = sum(folder.size() for folder in self.subfolders.values())
        # Cache the size for quick recovery
        self._size = files_size+subfolders_size
        return self._size

    def __str__(self):
        return self.name


class FileSystem:
    def __init__(self):
        self.home = Folder()
        self.current_dir = self.home

    def align_with_stream(self, stream):
        for line in stream:
            self.current_dir = self.current_dir.execute_line(line)


def flatten(home, f: Callable[[object], int] = lambda x: x._size) -> list:
    """Flatten a tree into a list.

    Create a list and extend it with flattened children.

    Args:
        home (Folder): The home folder
        f (Callable): Function to create represention of nodes

    Returns:
        list: List of flattened tree
    """
    if not hasattr(home, "_size"):
        home.size()
    flat = [f(home)]
    for folder in home.subfolders.values():
        flat.extend(flatten(folder, f))
    return flat


files = FileSystem()
files.align_with_stream(data)

files.home.size()
flat = flatten(files.home)

print(sum(size for size in flat if size <= 100000))

required_space = 30000000-(70000000-files.home._size)
print(min(size for size in flat if size >= required_space))
