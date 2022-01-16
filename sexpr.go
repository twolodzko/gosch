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
		sexpr, err := env.Get(val)
		if err != nil {
			return Sexpr{}, err
		}
		return sexpr.Eval(env)
	default:
		return s, nil
	}
}
