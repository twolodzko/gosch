package main

import "fmt"

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

func (p *Parser) ReadAtom() Sexpr {
	var runes []rune
	for p.HasNext() {
		if p.Head() == ' ' || p.Head() == '(' || p.Head() == ')' {
			break
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	if len(runes) > 0 {
		return Sexpr{string(runes)}
	} else {
		return Sexpr{}
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
		case ' ':
			p.pos++
		case ')':
			return newList(elems), nil
		default:
			elem, err := p.ReadNext()
			if err != nil {
				return List{}, err
			}
			elems = append(elems, elem)
		}
	}
	return List{}, fmt.Errorf("list was not closed with )")
}

func (p *Parser) ReadNext() (Sexpr, error) {
	switch p.Head() {
	case '(':
		list, err := p.ReadList()
		return Sexpr{list}, err
	// TODO: is this needed?
	case ')':
		return Sexpr{}, fmt.Errorf("unexpected )")
	default:
		return p.ReadAtom(), nil
	}
}

func (p *Parser) Parse() (Sexpr, error) {
	for p.HasNext() {
		switch p.Head() {
		case ' ':
			p.pos++
		default:
			return p.ReadNext()
		}
	}
	return Sexpr{}, nil
}
