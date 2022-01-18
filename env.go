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

func (env *Env) Eval(sexpr Sexpr) (Sexpr, error) {
	var err error
	for {
		if sexpr.Quoted {
			sexpr.Quoted = false
			return sexpr, nil
		}

		switch val := sexpr.Value.(type) {
		case *Pair:
			return env.EvalPair(val)
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
		return Sexpr{&Pair{}, false}, nil
	}

	args, err := env.EvalArgs(pair)
	if err != nil {
		return Sexpr{}, err
	}

	if name, ok := pair.This.Value.(string); ok {
		if fn, ok := buildin(name); ok {
			return fn(args)
		}
	}

	return Sexpr{}, fmt.Errorf("%v is not callable", pair.This)
}

func buildin(name string) (func(*Pair) (Sexpr, error), bool) {
	switch name {
	case "car":
		return car, true
	case "cdr":
		return cdr, true
	case "null?":
		return isNull, true
	case "pair?":
		return isPair, true
	case "cons":
		return cons, true
	case "list":
		return list, true
	default:
		return nil, false
	}
}