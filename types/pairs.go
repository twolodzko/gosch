package types

import "fmt"

type Pair struct {
	This Sexpr
	Next *Pair
}

func NewPair(this Sexpr, next Sexpr) *Pair {
	switch next := next.(type) {
	case *Pair:
		return &Pair{this, next}
	case nil:
		return &Pair{this, nil}
	default:
		return &Pair{this, &Pair{next, nil}}
	}
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

func (p *Pair) Len() int {
	if p.IsNull() {
		return 0
	}

	var len int
	head := p
	for head != nil {
		len++
		head = head.Next
	}
	return len
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

type AppendablePair struct {
	pair *Pair
	Last *Pair
}

func NewAppendablePair() *AppendablePair {
	head := &Pair{}
	return &AppendablePair{head, head}
}

func (p *AppendablePair) Append(sexpr Sexpr) {
	p.Last.Next = NewPair(sexpr, nil)
	p.Last = p.Last.Next
}

func (p AppendablePair) ToPair() *Pair {
	if p.pair.IsNull() {
		return &Pair{}
	}
	return p.pair.Next
}
