package main

type Sexpr struct {
	value interface{}
}

type List struct {
	this Sexpr
	next *List
}

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

func (p *Parser) Read() rune {
	val := p.str[p.pos]
	p.pos++
	return val
}

func (p *Parser) ReadAtom() Sexpr {
	var runes []rune
	for p.HasNext() {
		runes = append(runes, p.Read())
	}
	return Sexpr{string(runes)}
}

func (p *Parser) ReadList() (Sexpr, error) {
	var list List
	// for p.HasNext() {

	// }
	return Sexpr{list}, nil
}

func (p *Parser) Parse() (Sexpr, error) {
	for p.HasNext() {
		switch p.str[p.pos] {
		case ' ':
			// skip
		case '(':
			return p.ReadList()
		default:
			return p.ReadAtom(), nil
		}
		p.pos++
	}
	return Sexpr{}, nil
}
