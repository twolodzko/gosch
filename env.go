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

func (env *Env) Eval(sexpr Any) (Any, error) {
	var err error
	for {
		switch val := sexpr.(type) {
		case *Pair:
			return env.evalPair(val)
		case string:
			sexpr, err = env.Get(val)
			if err != nil {
				return nil, err
			}
		default:
			return sexpr, nil
		}
	}
}

func (env *Env) evalAll(pair *Pair) (*Pair, error) {
	var (
		head *Pair
		args []Any
	)
	head = pair
	for head != nil {
		sexpr, err := env.Eval(head.This)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
		head = head.Next
	}
	// TODO: avoid re-packing
	return newPair(args), nil
}

func (env *Env) evalPair(pair *Pair) (Any, error) {
	if pair.IsNull() {
		return &Pair{}, nil
	}

	var (
		err   error
		first Any = pair.This
	)
	for {
		name, ok := first.(string)
		if !ok {
			return nil, fmt.Errorf("%v is not callable", first)
		}

		switch name {
		case "define":
			return env.define(pair.Next)
		case "quote":
			return pair.Next.This, nil
		default:
			if fn, ok := buildin(name); ok {
				args, err := env.evalAll(pair.Next)
				if err != nil {
					return nil, err
				}
				return fn(args)
			} else {
				first, err = env.Get(name)
				if err != nil {
					return nil, err
				}
			}
		}
	}
}
