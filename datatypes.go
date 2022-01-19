package main

import "fmt"

type Bool bool
type Any = interface{}

type Pair struct {
	This Any
	Next *Pair
}

func (p Pair) HasNext() bool {
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

func newPair(elems []Any) *Pair {
	switch len(elems) {
	case 0:
		return &Pair{}
	case 1:
		return &Pair{elems[0], nil}
	default:
		this := elems[0]
		next := newPair(elems[1:])
		return &Pair{this, next}
	}
}

func (b Bool) String() string {
	if b {
		return "#t"
	}
	return "#f"
}

func isTrue(s Any) Bool {
	switch val := s.(type) {
	case Bool:
		return val
	default:
		return true
	}
}
