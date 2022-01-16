package main

import (
	"fmt"
	"io"
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

func (p *Parser) ReadAtomValue() (string, error) {
	var runes []rune
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) || p.Head() == '(' || p.Head() == ')' {
			break
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	if len(runes) > 0 {
		return string(runes), nil
	} else {
		return "", fmt.Errorf("nothing was read")
	}
}

func (p *Parser) ReadPair() (Pair, error) {
	p.pos++
	var sexprs []Sexpr
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
			continue
		}

		switch p.Head() {
		case ')':
			p.pos++
			return newPair(sexprs), nil
		default:
			elem, err := p.ReadSexpr()
			if err != nil {
				return Pair{}, err
			}
			sexprs = append(sexprs, elem)
		}
	}
	return Pair{}, fmt.Errorf("list was not closed with )")
}

func (p *Parser) ReadSexpr() (Sexpr, error) {
	quote := false
	for p.HasNext() {
		switch p.Head() {
		case '\'':
			quote = true
			p.pos++
		case '(':
			list, err := p.ReadPair()
			return Sexpr{list, quote}, err
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
			continue
		}

		sexpr, err := p.ReadSexpr()
		if err != nil {
			return nil, err
		}
		sexprs = append(sexprs, sexpr)
	}
	return sexprs, nil
}
