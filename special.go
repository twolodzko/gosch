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

func let(args *Pair, env *Env) (Any, error) {
	local := NewEnv()
	local.parent = env

	bindings, ok := args.This.(*Pair)
	if !ok {
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
	// iterate through the bindings ((name1 value1) (name2 value2) ...)
	head := bindings
	for head != nil {
		binding, ok := head.This.(*Pair)
		if !ok {
			return nil, fmt.Errorf("%v is not a list", head.This)
		}
		// unpack binding (name value)
		if name, ok := binding.This.(string); ok {
			if !binding.HasNext() {
				return nil, fmt.Errorf("%v has not value to bind", binding)
			}
			local.Set(name, binding.Next.This)
		} else {
			return nil, fmt.Errorf("binding %v does not use proper name", binding)
		}
		head = head.Next
	}

	if !args.HasNext() {
		return nil, nil
	}
	body := args.Next

	pair, err := evalAll(body, local)
	return pair.Last(), err
}

// func lambda(args *Pair, env *Env) (Any, error) {

// }
