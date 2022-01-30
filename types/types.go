package types

import "fmt"

type (
	Sexpr  = interface{}
	Bool   bool
	Symbol = string
	String string
)

type Pair struct {
	This Sexpr
	Next *Pair
}

func NewPair(this Sexpr, next *Pair) *Pair {
	return &Pair{this, next}
}

func (p Pair) HasNext() Bool {
	return p.Next != nil
}

func (p Pair) IsNull() Bool {
	return p == (Pair{})
}

func (p *Pair) Cons(sexpr Sexpr) *Pair {
	if p.IsNull() {
		return &Pair{sexpr, nil}
	}
	return &Pair{sexpr, p}
}

func (p Pair) String() string {
	if p.IsNull() {
		return "()"
	}

	var elems string
	for {
		elems += fmt.Sprintf("%v", p.This)
		if !p.HasNext() {
			break
		}
		p = *p.Next
		elems += " "
	}
	return fmt.Sprintf("(%s)", elems)
}

func PairFromArray(elems []Sexpr) *Pair {
	switch len(elems) {
	case 0:
		return &Pair{}
	case 1:
		return &Pair{elems[0], nil}
	default:
		this := elems[0]
		next := PairFromArray(elems[1:])
		return &Pair{this, next}
	}
}

func (b Bool) String() string {
	if b {
		return "#t"
	}
	return "#f"
}

func IsTrue(s Sexpr) Bool {
	switch val := s.(type) {
	case Bool:
		return val
	default:
		return true
	}
}

func Quote(s Sexpr) Sexpr {
	return &Pair{"quote", &Pair{s, nil}}
}

func (s String) String() string {
	return `"` + string(s) + `"`
}
