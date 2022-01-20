package main

import "fmt"

type Env struct {
	vars map[string]Any
}

func NewEnv() *Env {
	vars := make(map[string]Any)
	return &Env{vars}
}

func (e *Env) Set(name string, value Any) {
	e.vars[name] = value
}

func (e *Env) Get(name string) (Any, error) {
	value, ok := e.vars[name]
	if !ok {
		return nil, fmt.Errorf("unbound variable %v", name)
	}
	return value, nil
}
