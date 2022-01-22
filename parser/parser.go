package parser

import (
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

func (p *Parser) Read() ([]types.Any, error) {
	var sexprs []types.Any
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
			return types.Bool(true), nil
		case str == "#f":
			return types.Bool(false), nil
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

func (p *Parser) readPair() (*types.Pair, error) {
	p.pos++
	var sexprs []types.Any
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

func (p *Parser) readSexpr() (types.Any, error) {
	var (
		val types.Any
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
				val = types.Quote(val)
			}
			return val, err
		case ')':
			return nil, fmt.Errorf("unexpected )")
		default:
			val, err = p.readAtomValue()
			for i := 0; i < quotes; i++ {
				val = types.Quote(val)
			}
			return val, err
		}
	}
	return nil, io.EOF
}