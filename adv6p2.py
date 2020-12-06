#! /usr/bin/env python3
# may need python3.X on some BSDs
# in hindsight, this should have been the awk one I guess.
import sys

# questions are a-z
# each line lists the questions that got a "yes" from some person
# eg "abc" => answered yes to a,b,c
# groups of people are separated by empty lines, eg
# a
# b
#
# c => (a,b) group, then (c) group
# => foreach group, #questions that got >=1 yes => sum that

# part1: need to identify questions to which *ANYONE* answered yes!
# part2: need to identify questions to which EVERYONE answered yes!

# start with all set to true...
def newgroup():
    s = set()
    for i in range(256): # wasted stupid amount of time in p1
        if i >= ord('a') and i <= ord('z'):
            s.add(chr(i))
    return s
def groupadd(group, line):
    for candidate in set(group): # can't change set while iterating!
        if candidate in line:
            continue
        group.discard(candidate)

g = newgroup()
tot = 0
for line in sys.stdin.readlines():
    if line == "\n" or line == "":
        tot += len(g)
        g = newgroup()
    else:
        groupadd(g, line)
tot += len(g)
print(tot)

