package main

import (
	"fmt"
	"io"
	"regexp"
	"strconv"
	"unicode"
)

type Parser struct {
	str []rune
	pos int
}

func newParser(str string) *Parser {
	return &Parser{[]rune(str), 0}
}

func (p *Parser) HasNext() bool {
	return p.pos < len(p.str)
}

func (p *Parser) Head() rune {
	return p.str[p.pos]
}

func (p *Parser) Read() ([]Any, error) {
	var sexprs []Any
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
		} else {
			sexpr, err := p.readSexpr()
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, sexpr)
		}
	}
	return sexprs, nil
}

func (p *Parser) readAtomValue() (interface{}, error) {
	var runes []rune
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) || p.Head() == '(' || p.Head() == ')' {
			break
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	if len(runes) > 0 {
		str := string(runes)
		switch {
		case isInt(str):
			return strconv.Atoi(str)
		case str == "#t":
			return Bool(true), nil
		case str == "#f":
			return Bool(false), nil
		default:
			return str, nil
		}
	} else {
		return nil, fmt.Errorf("nothing was read")
	}
}

func isInt(str string) bool {
	matched, _ := regexp.MatchString(`^[+-]?\d+$`, str)
	return matched
}

func (p *Parser) readPair() (*Pair, error) {
	p.pos++
	var sexprs []Any
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return newPair(sexprs), nil
		default:
			elem, err := p.readSexpr()
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, elem)
		}
	}
	return nil, fmt.Errorf("pair was not closed with )")
}

func (p *Parser) readSexpr() (Any, error) {
	var (
		val Any
		err error
	)
	quotes := 0
	for p.HasNext() {
		switch p.Head() {
		case '\'':
			quotes++
			p.pos++
		case '(':
			val, err = p.readPair()
			for i := 0; i < quotes; i++ {
				val = quote(val)
			}
			return val, err
		case ')':
			return nil, fmt.Errorf("unexpected )")
		default:
			val, err = p.readAtomValue()
			for i := 0; i < quotes; i++ {
				val = quote(val)
			}
			return val, err
		}
	}
	return nil, io.EOF
}

func quote(sexpr Any) Any {
	return &Pair{"quote", &Pair{sexpr, nil}}
}
