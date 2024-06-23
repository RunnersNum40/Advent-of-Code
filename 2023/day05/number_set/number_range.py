import logging
from typing import List


class NumberRange:
    """
    A range of numbers.
    """

    def __init__(self, start: int, end: int) -> None:
        self.start = start
        self.end = end

        if len(self) < 0:
            logging.debug(
                f"NumberRange created with negative length: {self.start} to {self.end}"
            )

        if len(self) == 0:
            logging.debug(
                f"NumberRange created with zero length: {self.start} to {self.end}"
            )

    def __len__(self) -> int:
        return self.end - self.start

    def offset(self, increment: int) -> None:
        self.start += increment
        self.end += increment

    def __contains__(self, number: int) -> bool:
        return self.start <= number < self.end

    def __repr__(self) -> str:
        return f"NumberRange({self.start}, {self.end})"


def range_union(range1: NumberRange, range2: NumberRange) -> List[NumberRange]:
    """Return the union of two ranges."""
    if range1.end <= range2.start or range2.end <= range1.start:
        return [range1, range2]
    else:
        return [
            NumberRange(min(range1.start, range2.start), max(range1.end, range2.end))
        ]


def range_intersection(range1: NumberRange, range2: NumberRange) -> NumberRange:
    """Return the intersection of two ranges."""
    if range1.end <= range2.start or range2.end <= range1.start:
        return None
    else:
        return NumberRange(max(range1.start, range2.start), min(range1.end, range2.end))


def range_difference(range1: NumberRange, range2: NumberRange) -> List[NumberRange]:
    """Return the difference of two ranges (range1 - range2)."""
    # If there's no overlap
    if range1.end <= range2.start or range2.end <= range1.start:
        return [range1]

    # If range2 completely covers range1
    if range2.start <= range1.start and range1.end <= range2.end:
        return []

    # If range1 starts before range2
    if range1.start < range2.start:
        if range1.end <= range2.end:
            return [NumberRange(range1.start, range2.start)]
        else:
            return [
                NumberRange(range1.start, range2.start),
                NumberRange(range2.end, range1.end),
            ]

    # If range1 starts after range2 starts but ends after range2
    if range1.start >= range2.start and range1.end > range2.end:
        return [NumberRange(range2.end, range1.end)]

    # Default case, should not reach here in a valid scenario
    return []


def reduce_ranges(ranges: List[NumberRange]) -> List[NumberRange]:
    """
    Reduces a list of ranges by merging overlapping and adjacent ranges.
    """
    if not ranges:
        return []

    # Sort the ranges by their start value
    sorted_ranges = sorted(ranges, key=lambda r: r.start)

    # Reduce the ranges
    reduced_ranges = [sorted_ranges[0]]
    for range in sorted_ranges[1:]:
        if range.start <= reduced_ranges[-1].end:
            reduced_ranges[-1] = NumberRange(
                reduced_ranges[-1].start, max(reduced_ranges[-1].end, range.end)
            )
        else:
            reduced_ranges.append(range)

    return reduced_ranges
