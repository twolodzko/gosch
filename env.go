package main

import "fmt"

type Env struct {
	vars   map[string]Any
	parent *Env
}

func NewEnv() *Env {
	vars := make(map[string]Any)
	return &Env{vars, nil}
}

func (e *Env) Set(name string, value Any) {
	e.vars[name] = value
}

// Find an enclosing environment for the variable
func (e *Env) FindEnv(name string) (*Env, bool) {
	current := e
	for current != nil {
		if _, ok := current.vars[name]; ok {
			return current, true
		}
		current = current.parent
	}
	return nil, false
}

func (e *Env) Get(name string) (Any, error) {
	if env, ok := e.FindEnv(name); ok {
		return env.vars[name], nil
	}
	return nil, fmt.Errorf("unbound variable %v", name)
}
