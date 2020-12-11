// TODO too much repetition; no table driven, no nothing.
package main

// As soon as people start to arrive, you realize your mistake.
// People don't just care about adjacent seats - they care about the first seat they can see in each of those eight directions!
// (think of a queen in a game of chess)

// eg: .L.L.#.#.#.#.
// => left sees all free, right sees taken to the right.

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

// as in part1
func (f *Ferry) Adiacent1(i, j int) int {
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
			if f.rows[newi][newj] == occupied {
				n++
			}
		}
	}
	return n
}

const outOfBounds = '!'

func wtf(f *Ferry, i, j int) byte {
	if i < 0 || i >= len(f.rows) {
		return outOfBounds
	}
	if j < 0 || j >= len(f.rows[i]) {
		return outOfBounds
	}
	return f.rows[i][j]
}

// only one or zero; a 0-1 boolean would be ideal...
// otherwise rename to "oneIfFirstOccupiedElseZero" :D
func seatsInDirection(f *Ferry, i, j int, newi, newj func(int) int) int {
	ni := i
	nj := j
	for {
		ni = newi(ni)
		nj = newj(nj)
		if ni == i && nj == j {
			panic("copypasta bug - BOTH i and j unchanged")
		}
		s := wtf(f, ni, nj)
		switch s {
		case empty:
			return 0
		case occupied:
			return 1
		case floor:
			continue
		case outOfBounds:
			return 0
		default:
			panic("bug")
		}
	}
}

// as in part2 - TODO a decent name!
func (f *Ferry) Adiacent2(i, j int) int {
	_ = f.rows[i][j]
	inc1 := func(x int) int { return x + 1 }
	dec1 := func(x int) int { return x - 1 }
	same := func(x int) int { return x }
	n := 0
	n += seatsInDirection(f, i, j, inc1, same)
	n += seatsInDirection(f, i, j, same, inc1)
	n += seatsInDirection(f, i, j, dec1, same)
	n += seatsInDirection(f, i, j, same, dec1)
	n += seatsInDirection(f, i, j, inc1, inc1)
	n += seatsInDirection(f, i, j, dec1, inc1)
	n += seatsInDirection(f, i, j, inc1, dec1)
	n += seatsInDirection(f, i, j, dec1, dec1)
	return n
}
func max(a, b int) int {
	if a > b {
		return a
	}
	return b
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
// this is the version in part1
func (f *Ferry) Next1() Ferry {
	var next Ferry
	next.rows = make([]Row, len(f.rows))
	for i := range f.rows {
		next.rows[i] = f.rows[i].Copy()
		for j := range f.rows[i] {
			// empty, zero occupied adiacent seats => occupied
			if f.rows[i][j] == empty && f.Adiacent1(i, j) == 0 {
				next.rows[i][j] = occupied
				continue
			}
			// occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
			if f.rows[i][j] == occupied && f.Adiacent1(i, j) >= 4 {
				next.rows[i][j] = empty
				continue
			}
			// Otherwise, the seat's state does not change - copied above
		}
	}
	return next
}

func (f *Ferry) Next2() Ferry {
	var next Ferry
	next.rows = make([]Row, len(f.rows))
	for i := range f.rows {
		next.rows[i] = f.rows[i].Copy()
		for j := range f.rows[i] {
			// empty, zero occupied Adiacent2 seats => occupied
			if f.rows[i][j] == empty && f.Adiacent2(i, j) == 0 {
				next.rows[i][j] = occupied
				continue
			}
			// occupied (#) and FIVE or more seats Adiacent2 to it are also occupied, the seat becomes empty.
			if f.rows[i][j] == occupied && f.Adiacent2(i, j) >= 5 {
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
var part1flag *bool = flag.Bool("1", false, "run part 1")

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		println("stdin")
		args = []string{"/dev/stdin"}
	}
	for _, fn := range args {
		if *part1flag {
			part1(fn)
		} else {
			part2(fn)
		}
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
		next := ferry.Next1()
		if next.Equals(&ferry) {
			ferry = next
			break
		}
		ferry = next
	}
	fmt.Printf("%s\n\n", ferry.String())
	fmt.Printf("%s: part1 => %d seats occupied\n", filename, ferry.SeatsOccupied())
}


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
// => evolves to 26 occupied seats
func part2(filename string) {
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
		next := ferry.Next2()
		if next.Equals(&ferry) {
			ferry = next
			break
		}
		ferry = next
	}
	fmt.Printf("%s\n\n", ferry.String())
	fmt.Printf("%s: part2 => %d seats occupied\n", filename, ferry.SeatsOccupied())
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
