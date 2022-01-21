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
			case Lambda:
				sexpr, localEnv, err = fn.Call(val.Next, localEnv)
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
	var err error
	for {
		switch obj := sexpr.(type) {
		case string:
			if fn, ok := procedure(obj); ok {
				return fn, nil
			}
			if fn, ok := tcoProcedure(obj); ok {
				return fn, nil
			}

			sexpr, err = env.Get(obj)
			if err != nil {
				return nil, err
			}
		case Lambda:
			return obj, nil
		case *Pair:
			sexpr, err = Eval(obj, env)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not callable", obj)
		}
	}
}
