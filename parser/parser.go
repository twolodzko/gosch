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

func (p *Parser) Read() ([]types.Sexpr, error) {
	var sexprs []types.Sexpr
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
		} else {
			sexpr, err := p.readSexpr()
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

func (p *Parser) readAtom() (types.Sexpr, error) {
	var runes []rune
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) || p.Head() == '(' || p.Head() == ')' {
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

func parseAtom(str string) (types.Sexpr, error) {
	switch {
	case str == "#t":
		return types.Bool(true), nil
	case str == "#f":
		return types.Bool(false), nil
	case str == "nil":
		return nil, nil
	case isInt(str):
		return strconv.Atoi(str)
	case isFloat(str):
		return strconv.ParseFloat(str, 64)
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

func (p *Parser) readPair() (*types.Pair, error) {
	p.pos++
	var sexprs []types.Sexpr
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return types.PairFromArray(sexprs), nil
		default:
			elem, err := p.readSexpr()
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, elem)
		}
	}
	return nil, fmt.Errorf("list was not closed with )")
}

func (p *Parser) readString() (types.String, error) {
	p.pos++
	var runes []rune
	for p.HasNext() {
		if p.Head() == '\\' && len(p.str) > (p.pos+1) && p.str[p.pos+1] == '"' {
			runes = append(runes, '"')
			p.pos = p.pos + 2
			continue
		}
		if p.Head() == '"' {
			p.pos++
			return types.String(runes), nil
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

func (p *Parser) readSexpr() (types.Sexpr, error) {
	var (
		val types.Sexpr
		err error
	)
	quotes := 0
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
			continue
		}

		switch p.Head() {
		case '\'':
			quotes++
			p.pos++
		case '(':
			val, err = p.readPair()
			return quote(val, quotes), err
		case ')':
			return nil, fmt.Errorf("unexpected )")
		case '"':
			val, err = p.readString()
			return quote(val, quotes), err
		case ';':
			p.skipLine()
		default:
			val, err = p.readAtom()
			return quote(val, quotes), err
		}
	}
	return nil, io.EOF
}

func quote(s types.Sexpr, num int) types.Sexpr {
	for i := 0; i < num; i++ {
		s = types.Quote(s)
	}
	return s
}
