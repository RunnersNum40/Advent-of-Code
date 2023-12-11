# day1.py

from heapq import nlargest

file_name = "input.txt"
with open(file_name, "r") as file:
    data = [line for line in file]

elves = [[]]
for line in data:
    if line == "\n":
        elves.append([])
    else:
        elves[-1].append(int(line))

elves = list(map(sum, elves))

print(max(elves))
print(sum(nlargest(3, elves)))
