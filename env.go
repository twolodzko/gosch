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
		return nil, fmt.Errorf("unbound variable %v", name)
	}
	return value, nil
}

func (env *Env) Eval(sexpr Sexpr) (Sexpr, error) {
	var err error
	for {
		switch val := sexpr.(type) {
		case *Pair:
			return env.EvalPair(val)
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

func (env *Env) EvalArgs(pair *Pair) (*Pair, error) {
	var (
		head *Pair
		args []Sexpr
	)
	head = pair
	for head.Next != nil {
		head = head.Next
		sexpr, err := env.Eval(head.This)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
	}
	// TODO: avoid re-packing
	return newPair(args), nil
}

func (env *Env) EvalPair(pair *Pair) (Sexpr, error) {
	if pair.IsNull() {
		return &Pair{}, nil
	}

	var (
		err   error
		first Sexpr = pair.This
	)
	for {
		name, ok := first.(string)
		if !ok {
			return nil, fmt.Errorf("%v is not callable", first)
		}

		switch name {
		case "define":
			return env.Define(pair.Next)
		case "quote":
			return pair.Next.This, nil
		default:
			if fn, ok := buildin(name); ok {
				args, err := env.EvalArgs(pair)
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
