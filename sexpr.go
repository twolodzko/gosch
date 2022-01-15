package main

import "fmt"

type Sexpr struct {
	Value interface{}
}

func (s Sexpr) String() string {
	return fmt.Sprintf("%v", s.Value)
}
