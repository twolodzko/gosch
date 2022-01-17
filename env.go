package main

import (
	"errors"
	"fmt"
)

type Env struct {
	vars map[string]Sexpr
}

func NewEnv() *Env {
	vars := make(map[string]Sexpr)
	return &Env{vars}
}

func (e *Env) Set(name string, value Sexpr) {
	e.vars[name] = value
}

func (e *Env) Get(name string) (Sexpr, error) {
	value, ok := e.vars[name]
	if !ok {
		return Sexpr{}, fmt.Errorf("unbound variable %v", name)
	}
	return value, nil
}

func (env *Env) Eval(sexpr Sexpr) (Sexpr, error) {
	var err error
	for {
		if sexpr.Quoted {
			sexpr.Quoted = false
			return sexpr, nil
		}

		switch val := sexpr.Value.(type) {
		case Pair:
			return Sexpr{}, errors.New("not implemented")
		case string:
			sexpr, err = env.Get(val)
			if err != nil {
				return Sexpr{}, err
			}
		default:
			return sexpr, nil
		}
	}
}
