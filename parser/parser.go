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

func (p *Parser) readSexpr() (types.Sexpr, error) {
	for p.HasNext() {
		if unicode.IsSpace(p.Head()) {
			p.pos++
			continue
		}

		switch p.Head() {
		case '\'':
			p.pos++
			val, err := p.readSexpr()
			return Quote(val), err
		case '`':
			p.pos++
			val, err := p.readSexpr()
			return Quasiquote(val), err
		case ',':
			p.pos++
			val, err := p.readSexpr()
			return Unquote(val), err
		case '(':
			return p.readPair()
		case ')':
			return nil, fmt.Errorf("unexpected )")
		case '"':
			return p.readString()
		case ';':
			p.skipLine()
		case '#':
			if p.HasNext() && p.Following() == '(' {
				p.pos++
				return p.readVector()
			}
			fallthrough
		default:
			return p.readAtom()
		}
	}
	return nil, io.EOF
}

func (p *Parser) readAtom() (types.Sexpr, error) {
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

func parseAtom(str string) (types.Sexpr, error) {
	switch {
	case str == "#t":
		return types.TRUE, nil
	case str == "#f":
		return types.FALSE, nil
	case str == "nil":
		return nil, nil
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

func (p *Parser) readPair() (*types.Pair, error) {
	p.pos++
	ap := types.NewAppendablePair()
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return ap.ToPair(), nil
		default:
			elem, err := p.readSexpr()
			if err != nil {
				return nil, err
			}
			// ignore the dotted pair syntax
			if elem != "." {
				ap.Append(elem)
			}
		}
	}
	return nil, fmt.Errorf("list was not closed with )")
}

func (p *Parser) readVector() (*types.Vector, error) {
	p.pos++
	vec := types.Vector{}
	for p.HasNext() {
		switch {
		case unicode.IsSpace(p.Head()):
			p.pos++
		case p.Head() == ')':
			p.pos++
			return &vec, nil
		default:
			elem, err := p.readSexpr()
			if err != nil {
				return nil, err
			}
			vec = append(vec, elem)
		}
	}
	return nil, fmt.Errorf("vector was not closed with )")
}

func (p *Parser) readString() (types.String, error) {
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

func Quote(s types.Sexpr) types.Sexpr {
	return types.NewPair("quote", s)
}

func Quasiquote(s types.Sexpr) types.Sexpr {
	return types.NewPair("quasiquote", s)
}

func Unquote(s types.Sexpr) types.Sexpr {
	return types.NewPair("unquote", s)
}

func isWordBoundary(r rune) bool {
	return unicode.IsSpace(r) || r == '(' || r == ')'
}
