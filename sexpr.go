package main

import (
	"errors"
	"fmt"
)

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

func (s Sexpr) Eval(env *Env) (Sexpr, error) {
	if s.Quoted {
		s.Quoted = false
		return s, nil
	}

	switch val := s.Value.(type) {
	case Pair:
		return Sexpr{}, errors.New("not implemented")
	case string:
		return env.Get(val)
	default:
		return s, nil
	}
}
