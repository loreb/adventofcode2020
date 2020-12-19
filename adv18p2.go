// For some reason I've always only done this things in lisp/sml,
// never in a language like this without using yacc etc (I think);
// so use go just because it has no unions or pattern matching etc.
// really, both lisp and sml would be ideal here.
package main

/*
--- Part Two ---
You manage to answer the child's questions and they finish part 1 of their homework, but get stuck when they reach the next section: advanced math.

Now, addition and multiplication have different precedence levels, but they're not the ones you're familiar with. Instead, addition is evaluated before multiplication.

For example, the steps to evaluate the expression 1 + 2 * 3 + 4 * 5 + 6 are now as follows:

1 + 2 * 3 + 4 * 5 + 6
  3   * 3 + 4 * 5 + 6
  3   *   7   * 5 + 6
  3   *   7   *  11
     21       *  11
         231
Here are the other examples from above:

1 + (2 * 3) + (4 * (5 + 6)) still becomes 51.
2 * 3 + (4 * 5) becomes 46.
5 + (8 * 3 + 9 + 3 * 4 * 3) becomes 1445.
5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4)) becomes 669060.
((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2 becomes 23340.
What do you get if you add up the results of evaluating the homework problems using these new rules?
*/

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"os"
)

// I'm keeping the "reverse" hack because it's so atrocious that I love it!

func main() {
	if len(os.Args) != 2 {
		panic("usage: $0 filename")
	}
	// I set out to do the tests, then I read you had to have MULTIPLE lines...
	//content := slurp(os.Args[1])
	//println(content)
	//fmt.Printf("%v\n", eval(content))
	tot := uint64(0)
	lines := readlines(os.Args[1])
	for _, line := range lines {
		println(string(line))
		x := eval(line)
		fmt.Printf("\t==> %v\n", x)
		tot += x
	}
	fmt.Printf("TOTAL: %v\n", tot)
}

// https://blog.gopheracademy.com/advent-2014/parsers-lexers/
// because I'm a bit short on time, except the original uses an io.Reader.
type Token int

const (
	// Special tokens
	ILLEGAL Token = 666 //iota
	EOF           = -1
	WS            = ' '

	NUMBER = 'n'

	// combinators
	MULT       = '*'
	PLUS       = '+'
	OPENPAREN  = '('
	CLOSEPAREN = ')'
)

type Scanner struct {
	full []byte
	pos  int
}

func NewScanner(text []byte) *Scanner {
	return &Scanner{full: text, pos: 0}
}

const eof = rune(0)

// read reads the next rune from the bufferred reader.
// Returns the rune(0) if an error occurs (or io.EOF is returned).
func (s *Scanner) read() rune {
	if s.pos >= len(s.full) {
		return eof
	}
	ch := s.full[s.pos]
	s.pos++
	return rune(ch)
}
func (s *Scanner) unread() {
	if s.pos < 1 {
		panic("not unreadable")
	}
	s.pos--
}

// Scan returns the next token and literal value.
func (s *Scanner) Scan() (tok Token, lit string) {
	// Read the next rune.
	ch := s.read()

	// If we see whitespace then consume all contiguous whitespace.
	// If we see a letter then consume as an ident or reserved word.
	if isWhitespace(ch) {
		s.unread()
		return s.scanWhitespace()
	} else if isdigit(ch) {
		s.unread()
		return s.scanNumber()
	}

	// Otherwise read the individual character.
	switch ch {
	case eof:
		return EOF, ""
	case '*':
		return MULT, string(ch)
	case '+':
		return PLUS, string(ch)
	case '(':
		return OPENPAREN, string(ch)
	case ')':
		return CLOSEPAREN, string(ch)
	}

	return ILLEGAL, string(ch)
}

func (s *Scanner) scanWhitespace() (tok Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent whitespace character into the buffer.
	// Non-whitespace characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if !isWhitespace(ch) {
			s.unread()
			break
		} else {
			buf.WriteRune(ch)
		}
	}

	return WS, buf.String()
}
func isWhitespace(ch rune) bool {
	return ch == ' ' || ch == '\t' || ch == '\n'
}

func (s *Scanner) scanNumber() (tok Token, lit string) {
	// Create a buffer and read the current character into it.
	var buf bytes.Buffer
	buf.WriteRune(s.read())

	// Read every subsequent ident character into the buffer.
	// Non-ident characters and EOF will cause the loop to exit.
	for {
		if ch := s.read(); ch == eof {
			break
		} else if !isdigit(ch) {
			s.unread()
			break
		} else {
			_, _ = buf.WriteRune(ch)
		}
	}

	return NUMBER, buf.String()
}

// Parser represents a parser.
type Parser struct {
	s   *Scanner
	buf struct {
		tok Token  // last read token
		lit string // last read literal
		n   int    // buffer size (max=1)
	}
}

// NewParser returns a new instance of Parser.
func NewParser(text []byte) *Parser {
	return &Parser{s: NewScanner(text)}
}

// scan returns the next token from the underlying scanner.
// If a token has been unscanned then read that instead.
func (p *Parser) scan() (tok Token, lit string) {
	// If we have a token on the buffer, then return it.
	if p.buf.n != 0 {
		p.buf.n = 0
		return p.buf.tok, p.buf.lit
	}

	// Otherwise read the next token from the scanner.
	tok, lit = p.s.Scan()

	// Save it to the buffer in case we unscan later.
	p.buf.tok, p.buf.lit = tok, lit

	return
}

// unscan pushes the previously read token back onto the buffer.
func (p *Parser) unscan() { p.buf.n = 1 }

// scanIgnoreWhitespace scans the next non-whitespace token.
func (p *Parser) scanIgnoreWhitespace() (tok Token, lit string) {
	tok, lit = p.scan()
	if tok == WS {
		tok, lit = p.scan()
	}
	return
}

type Union struct {
	tok Token
	lit string
}

// I'm not building a syntax tree, tnx
// (I would have to go back to the book to avoid a long debugging session)
func eval(text []byte) uint64 {
	p := NewParser(text)
	var left []Union
	for {
		tok, lit := p.scanIgnoreWhitespace()
		if tok == EOF {
			break
		}
		u := Union{tok: tok, lit: lit}
		left = append(left, u)
	}
	return evalUnion(reverse(left))
}

func reverse(u []Union) []Union {
	n := len(u)
	r := make([]Union, n)
	for i := range u {
		j := n - i - 1
		r[j] = u[i]
		switch u[i].tok {
		case OPENPAREN:
			r[j].tok = CLOSEPAREN
		case CLOSEPAREN:
			r[j].tok = OPENPAREN
		}
	}
	return r
}

// While I do keep blaming a distracting environment,
// my first version did "1 op 2 op 3" => "1 op (2 op 3)"...
// I am so tempted to just "reverse" the expression and pass it to the original...
func evalUnion(left []Union) uint64 {
	if len(left) == 0 {
		panic("unexpected end")
	}

	u := left[0]
	fmt.Printf("eval %c %s\n", u.tok, u.lit)
	switch u.tok {
	case NUMBER:
		if len(left) == 1 {
			return tonum(u.lit)
		}
		return evalop(tonum(u.lit), left[1], left[2:])
	case OPENPAREN:
		closeparen := findmatchingparen(left)
		if closeparen == len(left)-1 {
			return evalUnion(left[1:closeparen])
		}
		return evalop(evalUnion(left[1:closeparen]), left[closeparen+1], left[closeparen+2:])
	default:
		panic(u.tok)
	}
}

func findmatchingparen(exp []Union) int {
	level := 0
	for i := range exp {
		if exp[i].tok == OPENPAREN {
			level++
			continue
		}
		if exp[i].tok == CLOSEPAREN {
			level--
			if level == 0 {
				return i
			}
		}
	}
	panic(fmt.Sprintf("unmatched paren in %v", exp))
}

// Remember: INVERTED PRECEDENCE!!!
func evalop(a uint64, u Union, left []Union) uint64 {
	fmt.Printf("%v (%s) %v", a, u.lit, left)
	switch u.tok {
	case MULT:
		return a * evalUnion(left)
	case PLUS:
		return evalplus(a, left)
	default:
		panic(u)
	}
}

func popaddend(left[]Union)(uint64,[]Union) {
	fmt.Printf("pop addend in %v\n", left)
	u := left[0]
	switch u.tok {
	case NUMBER:
		if len(left) == 1 {
			return tonum(u.lit), []Union{}
		}
		return tonum(u.lit), left[1:]
	case OPENPAREN:
		closeparen := findmatchingparen(left)
		if closeparen == len(left)-1 {
			return evalUnion(left[1:closeparen]), []Union{}
		}
		return evalUnion(left[1:closeparen]), left[closeparen+1:]
	default:
		panic(u.tok)
	}
}

// OMFG my brain is hardwired to do the right thing...
func evalplus(a1 uint64, left []Union) uint64 {
	a2,left := popaddend(left)
	if len(left) == 0 {
		return a1 + a2
	}
	return evalop(a1+a2, left[0], left[1:])
}

func readlines(filename string) [][]byte {
	raw, err := os.Open(filename)
	if err != nil {
		panic(err)
	}
	defer raw.Close()
	r := bufio.NewReader(raw)
	var ret [][]byte
	for {
		line, err := r.ReadBytes('\n')
		if err != nil {
			if err == io.EOF {
				break
			}
			panic(err)
		}
		if len(line) < 0 {
			panic("empty line")
		}
		if line[len(line)-1] == '\n' {
			line = line[:len(line)-1]
		}
		ret = append(ret, line)
	}
	if len(ret) < 0 {
		panic("empty file")
	}
	return ret
}

func isdigit(ch rune) bool {
	return '0' <= ch && ch <= '9'
}
func todigit(ch rune) uint64 {
	if !isdigit(ch) {
		panic(ch)
	}
	return uint64(ch - '0')
}
func tonum(s string) uint64 {
	x := uint64(0)
	for _, ch := range s {
		d := todigit(ch)
		y := x*10 + d
		if y < x {
			panic("overflow")
		}
		x = y
	}
	return x
}
