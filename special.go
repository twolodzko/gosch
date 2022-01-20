package main

import "fmt"

func define(args *Pair, env *Env) (Any, error) {
	switch name := args.This.(type) {
	case string:
		val, err := Eval(args.Next.This, env)
		if err != nil {
			return nil, err
		}
		env.Set(name, val)
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}
