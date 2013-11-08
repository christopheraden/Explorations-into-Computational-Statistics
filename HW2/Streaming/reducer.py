#!/usr/bin/env python

#from operator import itemgetter
import sys

current_pair = None
current_count = 0
xy_pair = None

# input comes from STDIN
for line in sys.stdin:
    line = line.strip()  # remove leading and trailing whitespace
    xy_pair, count = line.split('\t', 1)  # parse mapper.py input into key and value.

    try:  # convert count: str => int
        count = int(count)
    except ValueError:
        print("Could not coerce count into integer")
        continue  # If count not a number, we silently ignore and discard this line

    # this IF-switch only works because Hadoop sorts map output
    # by key (here: xy_pair) before it is passed to the reducer
    if current_pair == xy_pair:
        current_count += count
    else:
        if current_pair:
            string_list = current_pair.translate(None, '[],').split()  # Convert string to list of strings
            l = [float(x) for x in string_list]  # Convert list of string into list of floats.
            #We now have a list of floats: x_lo, x_hi, y_lo, y_hi. We print out string-coerced
            #versions of each float, separated by commas, then cat it with a string of the current count.
            #Result: "x_lo, x_hi, y_lo, y_hi, count" is printed to STDOUT.
            print('%s,%s' % (",".join(str(x) for x in l), str(current_count)))
        current_count = count
        current_pair = xy_pair

# Output the last pair
if current_pair == xy_pair:
    string_list = current_pair.translate(None, '[],').split()
    l = [float(x) for x in string_list]
    print('%s,%s' % (",".join(str(x) for x in l), str(current_count)))