import unittest
from number_set import (
    NumberRange,
    range_union,
    range_intersection,
    range_difference,
    reduce_ranges,
)
from number_set import NumberSet, set_union, set_intersection, set_difference


class TestNumberRange(unittest.TestCase):
    def test_number_range_length(self):
        nr = NumberRange(1, 5)
        self.assertEqual(len(nr), 4)

    def test_number_range_contains(self):
        nr = NumberRange(1, 5)
        self.assertTrue(3 in nr)
        self.assertFalse(5 in nr)

    # Add more tests for NumberRange...


class TestRangeOperations(unittest.TestCase):
    def test_range_union_no_overlap(self):
        r1 = NumberRange(1, 3)
        r2 = NumberRange(5, 7)
        self.assertEqual(range_union(r1, r2), [r1, r2])

    def test_range_intersection_no_overlap(self):
        r1 = NumberRange(1, 3)
        r2 = NumberRange(5, 7)
        self.assertEqual(range_intersection(r1, r2), NumberRange(0, 0))

    # Add more tests for range operations...


class TestNumberSet(unittest.TestCase):
    def test_number_set_initialization(self):
        ns = NumberSet([NumberRange(1, 3), NumberRange(5, 7)])
        self.assertEqual(len(ns), 2)

    def test_number_set_union(self):
        s1 = NumberSet([NumberRange(1, 3)])
        s2 = NumberSet([NumberRange(5, 7)])
        union = set_union(s1, s2)
        self.assertEqual(len(union), 2)

    # Add more tests for NumberSet...


# Add more test cases as needed...

if __name__ == "__main__":
    unittest.main()
