package main

import (
	"fmt"
)

type Bool bool

func (b Bool) String() string {
	if b {
		return "#t"
	}
	return "#f"
}

type Sexpr struct {
	Value interface{}
}

func (s Sexpr) String() string {
	return fmt.Sprintf("%v", s.Value)
}

func (s Sexpr) IsNil() Bool {
	return s == (Sexpr{})
}

func (s Sexpr) IsTrue() Bool {
	switch val := s.Value.(type) {
	case Bool:
		return val
	default:
		return true
	}
}
