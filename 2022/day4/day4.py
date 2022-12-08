import sys
from typing import List


def overlaps(intervals: List[List[int]]) -> bool:
    left_begin, left_end = intervals[0][0], intervals[0][1]
    right_begin, right_end = intervals[1][0], intervals[1][1]
    return (left_end >= right_begin and left_end >= right_end) or left_begin == right_begin


if __name__ == '__main__':
    total = 0
    for line in sys.stdin:
        intervals = line.split(",")
        intervals = [[int(part) for part in x.split('-')] for x in intervals]
        if intervals[1][0] < intervals[0][0]:
            intervals[0], intervals[1] = intervals[1], intervals[0]

        if overlaps(intervals):
            total += 1

    print(total)
