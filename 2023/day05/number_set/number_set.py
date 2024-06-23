from typing import List, Union
from .number_range import (
    NumberRange,
    range_union,
    range_intersection,
    range_difference,
    reduce_ranges,
)


class NumberSet:
    """
    A set of numbers stored as ranges
    """

    def __init__(self, ranges: List[NumberRange] = []) -> None:
        self.ranges = reduce_ranges(ranges)

    def insert(self, range: Union[NumberRange, List[NumberRange]]) -> None:
        try:
            self.ranges = reduce_ranges(self.ranges + range)
        except TypeError:
            self.ranges = reduce_ranges(self.ranges + [range])

    def offset(self, offset: int) -> None:
        for range in self.ranges:
            range.offset(offset)

    def __len__(self) -> int:
        return len(self.ranges)

    def __getitem__(self, key: int) -> NumberRange:
        return self.ranges[key]

    def __repr__(self) -> str:
        return f"NumberSet({self.ranges})"


def set_union(set1: NumberSet, set2: NumberSet) -> NumberSet:
    """Return the union of two sets."""
    return NumberSet(set1.ranges + set2.ranges)


def set_intersection(set1: NumberSet, set2: NumberSet) -> NumberSet:
    """Return the intersection of two sets."""
    intersection = NumberSet()
    index1 = index2 = 0
    while index1 < len(set1) and index2 < len(set2):
        range1, range2 = set1[index1], set2[index2]

        if range1.end < range2.end:
            index1 += 1
        else:
            index2 += 1

        window = range_intersection(range1, range2)
        if window is not None:
            intersection.insert(window)

    return intersection


def set_difference(set1: NumberSet, set2: NumberSet) -> NumberSet:
    """Return the difference of two sets (set1 - set2)."""
    difference = NumberSet()
    index1 = index2 = 0
    while index1 < len(set1) and index2 < len(set2):
        range1, range2 = set1[index1], set2[index2]

        if range1.end < range2.end:
            index1 += 1
        else:
            index2 += 1

        difference.insert(range_difference(range1, range2))

    return difference
