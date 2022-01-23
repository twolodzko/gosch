package types

import "fmt"

type (
	Bool   bool
	Any    = interface{}
	Symbol = string
	String string
)

type Pair struct {
	This Any
	Next *Pair
}

func NewPair(this Any, next *Pair) *Pair {
	return &Pair{this, next}
}

func (p Pair) HasNext() Bool {
	return p.Next != nil
}

func (p Pair) IsNull() Bool {
	return p == (Pair{})
}

func (p *Pair) Cons(sexpr Any) *Pair {
	if p.IsNull() {
		return &Pair{sexpr, nil}
	}
	return &Pair{sexpr, p}
}

func (p *Pair) Len() int {
	if p.IsNull() {
		return 0
	}
	length := 0
	head := p
	for head != nil {
		length++
		head = head.Next
	}
	return length
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

func PairFromArray(elems []Any) *Pair {
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

func IsTrue(s Any) Bool {
	switch val := s.(type) {
	case Bool:
		return val
	default:
		return true
	}
}

func Quote(sexpr Any) Any {
	return &Pair{"quote", &Pair{sexpr, nil}}
}

func (s String) String() string {
	return `"` + string(s) + `"`
}
