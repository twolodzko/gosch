package types

import (
	"fmt"
	"strings"
)

type Vector []Sexpr

func NewVector(size int) Vector {
	return make(Vector, size)
}

func (v Vector) String() string {
	var elems []string
	for _, x := range v {
		elems = append(elems, fmt.Sprintf("%v", x))
	}
	return fmt.Sprintf("[%s]", strings.Join(elems, " "))
}
