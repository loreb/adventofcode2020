#! /usr/bin/python3

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

class Infinite3DGrid:
    # representation: eg x=X,y=Y,z=Z is represented with
    # key = (X**2) * (Y**3) * (Z**5)
    # in a hash table (since coordinates could be anywhere)
    # Inactive cubes are IMPLICITLY specified - anything not active is
    # inactive, that's why we use a hashtable.
    # XXX hashtable instead of set in case part2 requires storing something.
    # XXX the moment I took a break I did realize that this is completely
    # unnecessary, and complicates everything, and negative coordinates etc.
    # Which is the reason for many silly "print" statements, and something
    # that static typing would have warned me about.
    # Leaving this here just as a note to myself to NEVER have a solution
    # looking for a problem! (or at least wrap it up in assertions)
    # TODO __statically__ typed languages with infinite precision?
    # http://rosettacode.org/wiki/Arbitrary-precision_integers_(included)
    def __init__(self):
        self.activecubes = {}
    def __repr__(self):
        if len(self.activecubes) == 0:
            return '(completely inactive)'
        # list of planes, ie hashtable keyed by the 'z' values
        planes = {}
        for (x,y,z) in self.activecubes:
            if not z in planes:
                planes[z] = []
            planes[z].append((x,y))
        ret = ""
        for z in sorted(planes.keys()):
            ret += f"\n{z=}\n"
            ret += plane2string(planes[z])
            ret += "\n"
        return ret

    def activate(self, x, y, z):
        self.activecubes[(x,y,z)] = Cube()

    def isactive(self, x, y, z):
        return (x,y,z) in self.activecubes

    def allneighbors(self, x, y, z):
        ofs = [-1,0,1]
        neighbors = []
        for i in ofs:
            for j in ofs:
                for k in ofs:
                    if (i,j,k) == (0,0,0):
                        continue
                    neighbors.append((x+i, y+j, z+k))
        assert len(neighbors) == 26
        return neighbors
    def countactiveneighbors(self, x, y, z):
        return len([n for n in self.allneighbors(x,y,z) if n in self.activecubes])


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
        for (x,y,z) in self.activecubes.keys():
            todo.add((x,y,z))
            for n in self.allneighbors(x,y,z):
                todo.add(n)
        new = Infinite3DGrid()
        #print(f"{todo=}")
        for (x,y,z) in todo:
            n = self.countactiveneighbors(x,y,z)
            #print(f"{(x,y,z)=} {n=}")
            if self.isactive(x,y,z):
                if n == 2 or n == 3:
                    #print("2 or 3")
                    new.activate(x,y,z)
                else:
                    # everything is ALREADY inactive
                    assert not new.isactive(x,y,z)
            else:
                if n == 3:
                    #print("inactive->active")
                    new.activate(x,y,z)
                else:
                    assert not new.isactive(x,y,z)
        return new

def gridFromFile(filename):
    g = Infinite3DGrid()
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
            g.activate(x,y,0)
    return g



def test1():
    g = Infinite3DGrid()
    g.activate(0,0,0)
    g.activate(1,0,0)
    g.activate(2,0,0)
    g.activate(2,1,0)
    g.activate(1,2,0)
    return g

import sys
if len(sys.argv) != 2:
    raise Exception("usage: $0 file.txt")
g = gridFromFile(sys.argv[1])
for z in range(6+1):
    #print(f"{g=}")
    print(f"after {z=} cycles {len(g.activecubes)=}")
    g = g.evolved()

