package main

import "fmt"

func EvalString(code string, env *Env) ([]Any, *Env, error) {
	var out []Any
	parser := newParser(code)
	sexprs, err := parser.Read()
	if err != nil {
		return nil, env, err
	}
	for _, sexpr := range sexprs {
		result, err := Eval(sexpr, env)
		if err != nil {
			return nil, env, err
		}
		out = append(out, result)
	}
	return out, env, err
}

func Eval(sexpr Any, env *Env) (Any, error) {
	var err error
	localEnv := env
	for {
		switch val := sexpr.(type) {
		case *Pair:
			if val.IsNull() {
				return &Pair{}, nil
			}

			callable, err := getCallable(val.This, localEnv)
			if err != nil {
				return nil, err
			}
			switch fn := callable.(type) {
			case Procedure:
				return fn(val.Next, localEnv)
			case TcoProcedure:
				sexpr, localEnv, err = fn(val.Next, localEnv)
				if err != nil {
					return nil, err
				}
			default:
				return nil, fmt.Errorf("%v is not callable", fn)
			}

		case string:
			sexpr, err = localEnv.Get(val)
			if err != nil {
				return nil, err
			}
		default:
			return sexpr, nil
		}
	}
}

func getCallable(sexpr Any, env *Env) (interface{}, error) {
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
		if fn, ok := tcoProcedure(name); ok {
			return fn, nil
		}

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
