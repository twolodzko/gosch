package main

import "fmt"

func (env *Env) define(args *Pair) (Any, error) {
	switch name := args.This.(type) {
	case string:
		val, err := env.Eval(args.Next.This)
		if err != nil {
			return nil, err
		}
		env.Set(name, val)
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}
