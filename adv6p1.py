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

def newgroup(): return set()
def groupadd(group, line):
    for ch in line:
        if ch == '\n':
            continue
        if ch < 'a' or ch > 'z':
            raise Exception(line)
        group.add(ch)

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

# I spent like 20 minutes trying to figure out wtf was wrong;
# turns out I submitted "6251" while my script output "6521"...
