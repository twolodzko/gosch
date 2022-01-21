package main

import "fmt"

func Eval(sexpr Any, env *Env) (Any, error) {
	var err error
	for {
		switch val := sexpr.(type) {
		case *Pair:
			if val.IsNull() {
				return &Pair{}, nil
			}

			fn, err := getCallable(val.This, env)
			if err != nil {
				return nil, err
			}
			return fn(val.Next, env)
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

func getCallable(sexpr Any, env *Env) (Procedure, error) {
	var (
		name string
		err  error
	)
	for {
		name, err = getName(sexpr, env)
		if err != nil {
			return nil, err
		}

		if fn, ok := procedure(name); ok {
			return fn, nil
		}

		// get TCO procedure
		// ...

		sexpr, err = env.Get(name)
		if err != nil {
			return nil, err
		}
	}
}

func getName(sexpr Any, env *Env) (string, error) {
	switch val := sexpr.(type) {
	case string:
		return val, nil
	default:
		obj, err := Eval(val, env)
		if err != nil {
			return "", err
		}
		if name, ok := obj.(string); ok {
			return name, nil
		}
		return "", fmt.Errorf("%v is not callable", obj)
	}
}
