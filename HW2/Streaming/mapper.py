#!/usr/bin/env python
import sys
from math import floor, ceil

#Take (x, y) from each line, figure out what bin they belong to by rounding to nearest tenth.

# input comes from STDIN (standard input)
for line in sys.stdin:
    line = line.strip()  # strip whitespaces

    nums = line.split()  # split the line into X-Y pairs
    x_lo = floor(float(nums[0]) * 10) / 10
    x_hi = ceil(float(nums[0]) * 10) / 10
    y_lo = floor(float(nums[1]) * 10) / 10
    y_hi = ceil(float(nums[1]) * 10) / 10
    box = [x_lo, x_hi, y_lo, y_hi]
    print ('%s \t %s' % (box, 1))