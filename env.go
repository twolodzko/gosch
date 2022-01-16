package main

import "fmt"

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
