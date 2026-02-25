package parser

import (
	"errors"
	"fmt"
	"io"
	"regexp"
	"strconv"
	"unicode"

	"github.com/twolodzko/gosch/types"
)

type Parser struct {
	str []rune
	pos int
}

func NewParser(str string) *Parser {
	return &Parser{[]rune(str), 0}
}

func (p *Parser) HasNext() bool {
	return p.pos < len(p.str)
}

func (p *Parser) Head() rune {
	return p.str[p.pos]
}

func (p *Parser) Following() rune {
	return p.str[p.pos+1]
}

func (p *Parser) Read() ([]any, error) {
	var sexprs []any
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
		} else {
			sexpr, err := p.Sexpr()
			if err != nil && err != io.EOF {
				return nil, err
			}
			if err == io.EOF {
				break
			}
			sexprs = append(sexprs, sexpr)
		}
	}
	return sexprs, nil
}

func (p *Parser) Sexpr() (any, error) {
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
			continue
		}

		switch p.Head() {
		case '\'':
			p.pos++
			val, err := p.Sexpr()
			return Quote(val), err
		case '`':
			p.pos++
			val, err := p.Sexpr()
			return quasiQuote(val), err
		case ',':
			p.pos++
			val, err := p.Sexpr()
			return unquote(val), err
		case '(':
			return p.readPair()
		case ')':
			return nil, fmt.Errorf("unexpected closing bracket")
		case '"':
			return p.readString()
		case ';':
			p.skipLine()
		default:
			return p.readAtom()
		}
	}
	return nil, io.EOF
}

func (p *Parser) readAtom() (any, error) {
	var runes []rune
	for p.HasNext() {
		if isWordBoundary(p.Head()) {
			break
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	if len(runes) > 0 {
		return parseAtom(string(runes))
	} else {
		return nil, errors.New("nothing was read")
	}
}

func parseAtom(str string) (any, error) {
	switch {
	case str == "#t":
		return true, nil
	case str == "#f":
		return false, nil
	case isInt(str):
		val, err := strconv.Atoi(str)
		return types.Integer(val), err
	case isFloat(str):
		val, err := strconv.ParseFloat(str, 64)
		return types.Float(val), err
	default:
		return types.Symbol(str), nil
	}
}

func isInt(str string) bool {
	matched, _ := regexp.MatchString(`^[+-]?\d+$`, str)
	return matched
}

func isFloat(str string) bool {
	matched, _ := regexp.MatchString(`^[+-]?\d*\.?(?:\d+[eE]?[+-]?)?\d+$`, str)
	return matched
}
func (p *Parser) readPair() (any, error) {
	p.pos++
	var acc []any
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return types.List(acc...), nil
		case p.Head() == '.':
			p.pos++
			if p.Head() == ' ' {
				if len(acc) == 0 {
					return nil, fmt.Errorf("missing first pair element")
				}
				tail, err := p.Sexpr()
				if err != nil {
					return nil, err
				}
				pair := types.Cons(append(acc, tail)...)
				p.skipSpace()
				if p.Head() != ')' {
					return nil, fmt.Errorf("list was not closed with closing bracket")
				}
				p.pos++
				return pair, nil
			}
			// dot is a part of atom name
			p.pos--
			fallthrough
		default:
			elem, err := p.Sexpr()
			if err != nil {
				return nil, err
			}
			acc = append(acc, elem)
		}
	}
	return nil, fmt.Errorf("list was not closed with closing bracket")
}

func (p *Parser) readString() (string, error) {
	p.pos++
	var runes []rune
	for p.HasNext() {
		if p.Head() == '\\' && p.HasNext() && p.Following() == '"' {
			runes = append(runes, '"')
			p.pos = p.pos + 2
			continue
		}
		if p.Head() == '"' {
			p.pos++
			return string(runes), nil
		}
		runes = append(runes, p.Head())
		p.pos++
	}
	return "", errors.New("string was not closed with \"")
}

func (p *Parser) skipLine() {
	for p.HasNext() {
		if p.Head() == '\n' {
			p.pos++
			return
		}
		p.pos++
	}
}

func (p *Parser) skipSpace() {
	for p.HasNext() {
		if !unicode.IsSpace(p.Head()) {
			break
		}
		p.pos++
	}
}

func Quote(s any) any {
	return types.List(types.Symbol("quote"), s)
}

func quasiQuote(s any) any {
	return types.List(types.Symbol("quasiquote"), s)
}

func unquote(s any) any {
	return types.List(types.Symbol("unquote"), s)
}

func isWordBoundary(r rune) bool {
	return unicode.IsSpace(r) || r == '(' || r == ')'
}
