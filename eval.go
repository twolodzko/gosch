package main

import "fmt"

func Eval(sexpr Any, env *Env) (Any, error) {
	var err error
	for {
		switch val := sexpr.(type) {
		case *Pair:
			return evalPair(val, env)
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

func evalAll(pair *Pair, env *Env) (*Pair, error) {
	var (
		head *Pair
		args []Any
	)
	head = pair
	for head != nil {
		sexpr, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
		head = head.Next
	}
	// TODO: avoid re-packing
	return newPair(args), nil
}

func evalPair(pair *Pair, env *Env) (Any, error) {
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
			return define(pair.Next, env)
		case "quote":
			return pair.Next.This, nil
		default:
			if fn, ok := procedure(name); ok {
				args, err := evalAll(pair.Next, env)
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
