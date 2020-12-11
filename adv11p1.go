package main

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io"
	"os"
)

// The seat layout fits neatly on a grid. Each position is either floor (.), an empty seat (L), or an occupied seat (#).
// For example, the initial seat layout might look like this:
// L.LL.LL.LL
// LLLLLLL.LL
// L.L.L..L..
// LLLL.LL.LL
// L.LL.LL.LL
// L.LLLLL.LL
// ..L.L.....
// LLLLLLLLLL
// L.LLLLLL.L
// L.LLLLL.LL
// => evolves to 37 seats occupied.
const (
	empty    = 'L'
	floor    = '.'
	occupied = '#'
)

type Row []byte

func (r Row) Copy() Row {
	c := Row(make([]byte, len(r)))
	copy(c, r)
	return c
}
func (r Row) Equals(other Row) bool {
	return bytes.Compare([]byte(r), []byte(other)) == 0
}

type Ferry struct {
	rows []Row
}

func (f *Ferry) Adiacent(i, j int) int {
	_ = f.rows[i][j]
	n := 0
	for di := -1; di <= 1; di++ {
		for dj := -1; dj <= 1; dj++ {
			if di == 0 && dj == 0 {
				continue
			}
			newi := i + di
			newj := j + dj
			if newi < 0 || newi >= len(f.rows) {
				continue
			}
			if newj < 0 || newj >= len(f.rows[i]) {
				continue
			}
			//fmt.Printf("#rows=%d; len row=%d; ", len(f.rows), len(f.rows[i]))
			//fmt.Printf("i=%d j=%d newi=%d newj=%d\n", i, j, newi, newj)
			if f.rows[newi][newj] == occupied {
				n++
			}
		}
	}
	return n
}

func (f *Ferry) Equals(other *Ferry) bool {
	if len(f.rows) != len(other.rows) {
		panic("!= len")
	}
	for i := range f.rows {
		if f.rows[i].Equals(other.rows[i]) {
			continue
		}
		return false
	}
	return true
}
func (f *Ferry) SeatsOccupied() int {
	n := 0
	for i := range f.rows {
		for _, x := range f.rows[i] {
			if x == occupied {
				n++
			}
		}
	}
	return n
}

// evolve one step; "The following rules are applied to every seat simultaneously"
func (f *Ferry) Next() Ferry {
	var next Ferry
	next.rows = make([]Row, len(f.rows))
	for i := range f.rows {
		next.rows[i] = f.rows[i].Copy()
		for j := range f.rows[i] {
			// empty, zero occupied adiacent seats => occupied
			if f.rows[i][j] == empty && f.Adiacent(i, j) == 0 {
				next.rows[i][j] = occupied
				continue
			}
			// occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
			if f.rows[i][j] == occupied && f.Adiacent(i, j) >= 4 {
				next.rows[i][j] = empty
				continue
			}
			// Otherwise, the seat's state does not change - copied above
		}
	}
	return next
}

func (f *Ferry) String() string {
	ret := ""
	for i := range f.rows {
		ret += string([]byte(f.rows[i][:]))
		ret += "\n"
	}
	return ret
}

var debug *bool = flag.Bool("d", false, "debug print")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		println("stdin")
		args = []string{"/dev/stdin"}
	}
	for _, fn := range args {
		part1(fn)
	}
}

func part1(filename string) {
	r, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer r.Close()
	ferry := ferryFromReader(bufio.NewReader(r))
	// Simulate your seating area by applying the seating rules repeatedly until no seats change state.
	// How many seats end up occupied?
	for {
		if *debug {
			fmt.Printf("%s\n\n", ferry.String())
		}
		next := ferry.Next()
		if next.Equals(&ferry) {
			ferry = next
			break
		}
		ferry = next
	}
	fmt.Printf("%s\n\n", ferry.String())
	fmt.Printf("%s: %d seats occupied\n", filename, ferry.SeatsOccupied())
}

func chomp(line []byte) []byte {
	if line[len(line)-1] == '\n' {
		return line[:len(line)-1]
	}
	return line
}

func ferryFromReader(r *bufio.Reader) Ferry {
	var rows []Row
	linelen := -1 // all same line I hope...
	for {
		line, err := r.ReadBytes('\n')
		if err != nil {
			if err == io.EOF {
				break
			}
			panic(err)
		}
		line = chomp(line)
		if linelen < 0 {
			linelen = len(line)
		}
		if len(line) != linelen {
			panic(line)
		}
		rows = append(rows, rowFromLine(line))
	}
	var f Ferry
	f.rows = rows
	return f
}

func rowFromLine(line []byte) Row {
	for i := range line {
		switch line[i] {
		case empty:
			break
		case floor:
			break
		case occupied:
			break
		default:
			panic(line)
		}
	}
	r := Row(make([]byte, len(line)))
	copy(r[:], line)
	return r
}
