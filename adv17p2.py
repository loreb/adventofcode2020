#! /usr/bin/python3

"""
--- Part Two ---
For some reason, your simulated results don't match what the experimental energy source engineers expected.
Apparently, the pocket dimension actually has four spatial dimensions, not three.

The pocket dimension contains an infinite 4-dimensional grid.
At every integer 4-dimensional coordinate (x,y,z,w), there exists a single cube (really, a hypercube) which is still either active or inactive.

Each cube only ever considers its neighbors: any of the 80 other cubes where any of their coordinates differ by at most 1.
For example, given the cube at x=1,y=2,z=3,w=4, its neighbors include the cube at x=2,y=2,z=3,w=3, the cube at x=0,y=2,z=3,w=4, and so on.

The initial state of the pocket dimension still consists of a small flat region of cubes.
Furthermore, the same rules for cycle updating still apply: during each cycle, consider the number of active neighbors of each cube.

For example, consider the same initial state as in the example above.
Even though the pocket dimension is 4-dimensional, this initial state represents a small 2-dimensional slice of it.
(In particular, this initial state defines a 3x3x1x1 region of the 4-dimensional space.)

Simulating a few cycles from this initial state produces the following configurations, where the result of each cycle is shown layer-by-layer at each given z and w coordinate:

Before any cycles:

z=0, w=0
.#.
..#
###


After 1 cycle:

z=-1, w=-1
#..
..#
.#.

z=0, w=-1
#..
..#
.#.

z=1, w=-1
#..
..#
.#.

z=-1, w=0
#..
..#
.#.

z=0, w=0
#.#
.##
.#.

z=1, w=0
#..
..#
.#.

z=-1, w=1
#..
..#
.#.

z=0, w=1
#..
..#
.#.

z=1, w=1
#..
..#
.#.


After 2 cycles:

z=-2, w=-2
.....
.....
..#..
.....
.....

z=-1, w=-2
.....
.....
.....
.....
.....

z=0, w=-2
###..
##.##
#...#
.#..#
.###.

z=1, w=-2
.....
.....
.....
.....
.....

z=2, w=-2
.....
.....
..#..
.....
.....

z=-2, w=-1
.....
.....
.....
.....
.....

z=-1, w=-1
.....
.....
.....
.....
.....

z=0, w=-1
.....
.....
.....
.....
.....

z=1, w=-1
.....
.....
.....
.....
.....

z=2, w=-1
.....
.....
.....
.....
.....

z=-2, w=0
###..
##.##
#...#
.#..#
.###.

z=-1, w=0
.....
.....
.....
.....
.....

z=0, w=0
.....
.....
.....
.....
.....

z=1, w=0
.....
.....
.....
.....
.....

z=2, w=0
###..
##.##
#...#
.#..#
.###.

z=-2, w=1
.....
.....
.....
.....
.....

z=-1, w=1
.....
.....
.....
.....
.....

z=0, w=1
.....
.....
.....
.....
.....

z=1, w=1
.....
.....
.....
.....
.....

z=2, w=1
.....
.....
.....
.....
.....

z=-2, w=2
.....
.....
..#..
.....
.....

z=-1, w=2
.....
.....
.....
.....
.....

z=0, w=2
###..
##.##
#...#
.#..#
.###.

z=1, w=2
.....
.....
.....
.....
.....

z=2, w=2
.....
.....
..#..
.....
.....
After the full six-cycle boot process completes, 848 cubes are left in the active state.

Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?
"""


class Cube: pass

def countfactor(number, factor):
    n = 0
    while number % factor == 0:
        #number /= factor -- this puzzled me for a while :D
        number = number // factor
        n += 1
    return n

def plane2string(plane): # plane is a list of (x,y) tuples
    minx = min(c[0] for c in plane)
    miny = min(c[1] for c in plane)
    maxx = max(c[0] for c in plane)
    maxy = max(c[1] for c in plane)
    offx = 0 - minx
    offy = 0 - minx
    single = ["."] * (maxx+1-minx)
    lines = [single[:] for i in range(maxy+1-miny)]
    for (x,y) in plane:
        print(str((x,y)))
        lines[y+offy][x+offx] = "#"
    ret = ""
    # building it's more natural to have the origin up left;
    # display it like a carthesian plane.
    for line in reversed(lines):
        ret += ("".join(line)) + "\n"
    return ret

# ARGH! 3d => 4d!
# OK, they didn't change the rules, so easy change :D
class Infinite4DGrid:
    def __init__(self):
        self.activecubes = {}
    def __repr__(self):
        if len(self.activecubes) == 0:
            return '(completely inactive)'
        # list of planes, ie hashtable keyed by the 'z' and 'w' values
        planes = {}
        for (x,y,z,w) in self.activecubes:
            if not (z,w) in planes:
                planes[(z,w)] = []
            planes[(z,w)].append((x,y))
        ret = ""
        for (z,w) in sorted(planes.keys()):
            ret += f"\n{z=} {w=}\n"
            ret += plane2string(planes[(z,w)])
            ret += "\n"
        return ret

    def activate(self, x, y, z, w):
        self.activecubes[(x,y,z, w)] = Cube()

    def isactive(self, x, y, z, w):
        return (x,y,z, w) in self.activecubes

    def allneighbors(self, x, y, z, w):
        ofs = [-1,0,1]
        neighbors = []
        for i in ofs:
            for j in ofs:
                for k in ofs:
                    for q in ofs:
                        if (i,j,k,q) == (0,0,0,0):
                            continue
                        neighbors.append((x+i, y+j, z+k, w+q))
        assert len(neighbors) == 80
        return neighbors
    def countactiveneighbors(self, x, y, z, w):
        return len([n for n in self.allneighbors(x,y,z, w) if n in self.activecubes])


    # During a cycle, all cubes simultaneously change their state according to
    # the following rules:
    #   * If a cube is active and exactly 2 or 3 of its neighbors are also
    #     active, the cube remains active. Otherwise, the cube becomes inactive.
    #   * If a cube is inactive but exactly 3 of its neighbors are active, the
    #     cube becomes active. Otherwise, the cube remains inactive.
    def evolved(self):
        """ create a NEW grid which is evolved 1 step """
        todo = set()
        # we need active cubes AND inactive cubes that are immediate neighbors.
        for (x,y,z,w) in self.activecubes.keys():
            todo.add((x,y,z,w))
            for n in self.allneighbors(x,y,z,w):
                todo.add(n)
        new = Infinite4DGrid()
        #print(f"{todo=}")
        for (x,y,z,w) in todo:
            n = self.countactiveneighbors(x,y,z,w)
            #print(f"{(x,y,z,w)=} {n=}")
            if self.isactive(x,y,z,w):
                if n == 2 or n == 3:
                    #print("2 or 3")
                    new.activate(x,y,z,w)
                else:
                    # everything is ALREADY inactive
                    assert not new.isactive(x,y,z,w)
            else:
                if n == 3:
                    #print("inactive->active")
                    new.activate(x,y,z,w)
                else:
                    assert not new.isactive(x,y,z,w)
        return new

def gridFromFile(filename):
    g = Infinite4DGrid()
    f = open(filename, "rb")
    def active(line):
        a = []
        start = 0
        while(start >= 0):
            pos = line.find(b"#", start)
            if pos < 0: break
            a.append(pos)
            start = pos+1
        return a
    rows = []
    for line in f.readlines():
        rows.append(active(line))
    f.close()
    rows = list(reversed(rows))
    for y in range(len(rows)):
        for x in rows[y]:
            g.activate(x,y,0,0)
    return g



def test1():
    g = Infinite4DGrid()
    g.activate(0,0,0,0)
    g.activate(1,0,0,0)
    g.activate(2,0,0,0)
    g.activate(2,1,0,0)
    g.activate(1,2,0,0)
    return g

import sys
if len(sys.argv) != 2:
    raise Exception("usage: $0 file.txt")
g = gridFromFile(sys.argv[1])
for z in range(6+1):
    #print(f"{g=}")
    print(f"after {z=} cycles {len(g.activecubes)=}")
    g = g.evolved()

raise Exception("TODO")
