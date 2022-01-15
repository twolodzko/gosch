package main

import "fmt"

type List struct {
	this Sexpr
	next *List
}

func (l *List) HasNext() bool {
	return l.next != nil
}

func (a List) Equal(b List) bool {
	for {
		if a.this != b.this {
			return false
		}
		if a.next == nil {
			return b.next == nil
		}
		if b.next == nil {
			return false
		}
		a = *a.next
		b = *b.next
	}
}

func (a List) String() string {
	var elems string
	for a.HasNext() {
		elems += fmt.Sprintf("%v", a.this)
		a = *a.next
		if a.next != nil {
			elems += " "
		}
	}
	return fmt.Sprintf("(%s)", elems)
}

func newList(elems []Sexpr) List {
	if len(elems) > 0 {
		head := elems[0]
		tail := newList(elems[1:])
		return List{head, &tail}
	}
	return List{}
}
