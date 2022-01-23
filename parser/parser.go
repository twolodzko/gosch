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
			if str == "nil" {
				return nil, nil
			}
			return types.Symbol(str), nil
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
	return "", io.EOF
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
			return quote(val, quotes), err
		case ')':
			return nil, fmt.Errorf("unexpected )")
		case '"':
			val, err = p.readString()
			return quote(val, quotes), err
		default:
			val, err = p.readAtomValue()
			return quote(val, quotes), err
		}
	}
	return nil, io.EOF
}

func quote(obj types.Any, num int) types.Any {
	for i := 0; i < num; i++ {
		obj = types.Quote(obj)
	}
	return obj
}
