package main

import "fmt"

type Sexpr struct {
	value interface{}
}

func (s Sexpr) String() string {
	return fmt.Sprintf("%v", s.value)
}
