package main

import "fmt"

type List struct {
	This Sexpr
	Next *List
}

func (l *List) HasNext() bool {
	return l.Next != nil
}

func (a List) String() string {
	var elems string
	for {
		elems += fmt.Sprintf("%v", a.This)
		if a.Next == nil {
			break
		}
		a = *a.Next
		elems += " "
	}
	return fmt.Sprintf("(%s)", elems)
}

func newList(elems []Sexpr) List {
	switch len(elems) {
	case 0:
		return List{}
	case 1:
		return List{elems[0], nil}
	default:
		this := elems[0]
		next := newList(elems[1:])
		return List{this, &next}
	}
}
