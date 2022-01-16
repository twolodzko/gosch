package main

import "fmt"

type Sexpr struct {
	Value  interface{}
	Quoted bool
}

func (s Sexpr) String() string {
	if s.Quoted {
		return fmt.Sprintf("'%v", s.Value)
	}
	return fmt.Sprintf("%v", s.Value)
}

func (s Sexpr) Eval() (Sexpr, error) {
	if s.Quoted {
		s.Quoted = false
		return s, nil
	}
	return Sexpr{}, fmt.Errorf("unbound variable %v", s)
}
