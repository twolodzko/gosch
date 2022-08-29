package types

import (
	"fmt"
	"strings"
)

type Pair struct {
	This Sexpr
	Next *Pair
}

func NewPair(elems ...Sexpr) *Pair {
	switch len(elems) {
	case 0:
		return &Pair{}
	case 1:
		if elems[0] == nil {
			return nil
		}
		return &Pair{elems[0], nil}
	default:
		this := elems[0]
		next := NewPair(elems[1:]...)
		return &Pair{this, next}
	}
}

func MakePair(this Sexpr, next Sexpr) *Pair {
	switch next := next.(type) {
	case *Pair:
		return &Pair{this, next}
	default:
		return NewPair(this, next)
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
	return fmt.Sprintf("(%s)", p.ElemsToString())
}

func (p Pair) ElemsToString() string {
	var elems []string
	head := &p
	for head != nil && !head.IsNull() {
		elems = append(elems, fmt.Sprintf("%v", head.This))
		head = head.Next
	}
	return strings.Join(elems, " ")
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
	p.Last.Next = NewPair(sexpr)
	p.Last = p.Last.Next
}

func (p *AppendablePair) Extend(tail *Pair) {
	if tail == nil || tail.IsNull() {
		return
	}
	p.Last.Next = tail
	head := p.Last.Next
	for {
		if !head.HasNext() {
			p.Last = head
			return
		}
		head = head.Next
	}
}

func (p AppendablePair) ToPair() *Pair {
	if p.pair.IsNull() {
		return &Pair{}
	}
	return p.pair.Next
}
