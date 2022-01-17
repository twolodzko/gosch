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
