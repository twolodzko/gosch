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

func (p *Parser) ReadAtomValue() (interface{}, error) {
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
		if isInt(str) {
			return strconv.Atoi(str)
		}
		return str, nil
	} else {
		return nil, fmt.Errorf("nothing was read")
	}
}

func isInt(str string) bool {
	matched, _ := regexp.MatchString(`^[+-]?\d+$`, str)
	return matched
}

func (p *Parser) ReadPair() (*Pair, error) {
	p.pos++
	var sexprs []Sexpr
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return newPair(sexprs), nil
		default:
			elem, err := p.ReadSexpr()
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, elem)
		}
	}
	return nil, fmt.Errorf("pair was not closed with )")
}

func (p *Parser) ReadSexpr() (Sexpr, error) {
	quote := false
	for p.HasNext() {
		switch p.Head() {
		case '\'':
			quote = true
			p.pos++
		case '(':
			pair, err := p.ReadPair()
			return Sexpr{pair, quote}, err
		case ')':
			return Sexpr{}, fmt.Errorf("unexpected )")
		default:
			atom, err := p.ReadAtomValue()
			return Sexpr{atom, quote}, err
		}
	}
	return Sexpr{}, io.EOF
}

func (p *Parser) Read() ([]Sexpr, error) {
	var sexprs []Sexpr
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
		} else {
			sexpr, err := p.ReadSexpr()
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, sexpr)
		}
	}
	return sexprs, nil
}
