package main

import (
	"fmt"
	"io"
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

func (p *Parser) ReadAtom() (Sexpr, error) {
	var runes []rune
	for p.HasNext() {
		if p.Head() == ' ' || p.Head() == '(' || p.Head() == ')' {
			break
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	if len(runes) > 0 {
		return Sexpr{string(runes)}, nil
	} else {
		return Sexpr{}, fmt.Errorf("nothing was read")
	}
}

func (p *Parser) ReadList() (List, error) {
	if (len(p.str) - p.pos) < 2 {
		return List{}, fmt.Errorf("too short for a list")
	}
	p.pos++
	var elems []Sexpr
	for p.HasNext() {
		switch p.Head() {
		case ')':
			return newList(elems), nil
		default:
			elem, err := p.ReadSexpr()
			if err != nil {
				return List{}, err
			}
			elems = append(elems, elem)
		}
	}
	return List{}, fmt.Errorf("list was not closed with )")
}

func (p *Parser) ReadSexpr() (Sexpr, error) {
	for p.HasNext() {
		switch p.Head() {
		case ' ':
			p.pos++
		case '(':
			list, err := p.ReadList()
			p.pos++
			return Sexpr{list}, err
		case ')':
			return Sexpr{}, fmt.Errorf("unexpected )")
		default:
			return p.ReadAtom()
		}
	}
	return Sexpr{}, io.EOF
}

func (p *Parser) Parse() (Sexpr, error) {
	for p.HasNext() {
		return p.ReadSexpr()
	}
	return Sexpr{}, nil
}
