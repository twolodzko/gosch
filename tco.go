package main

import (
	"errors"
	"fmt"
)

type TcoProcedure = func(*Pair, *Env) (Any, *Env, error)

func tcoProcedure(name string) (TcoProcedure, bool) {
	switch name {
	case "let":
		return let, true
	case "if":
		return ifFn, true
	case "begin":
		return partialEval, true
	default:
		return nil, false
	}
}

func let(args *Pair, env *Env) (Any, *Env, error) {
	local := NewEnv()
	local.parent = env

	// bind variables
	bindings, ok := args.This.(*Pair)
	if !ok {
		return nil, env, fmt.Errorf("%v is not a list", args.This)
	}
	err := setBindings(bindings, local)
	if err != nil {
		return nil, env, err
	}

	// no body
	if !args.HasNext() {
		return nil, env, nil
	}
	body := args.Next

	return partialEval(body, local)
}

// Iterate through the bindings ((name1 value1) (name2 value2) ...) and set them to an environment
func setBindings(bindings *Pair, env *Env) error {
	if bindings.IsNull() {
		return nil
	}

	head := bindings
	for head != nil {
		switch pair := head.This.(type) {
		case *Pair:
			err := bind(pair, env)
			if err != nil {
				return err
			}
		default:
			return fmt.Errorf("%v is not a list", head.This)
		}
		head = head.Next
	}
	return nil
}

func bind(binding *Pair, env *Env) error {
	if name, ok := binding.This.(string); ok {
		if !binding.HasNext() {
			return fmt.Errorf("%v has not value to bind", binding)
		}
		env.Set(name, binding.Next.This)
		return nil
	}
	return fmt.Errorf("binding %v does not use proper name", binding)
}

func ifFn(args *Pair, env *Env) (Any, *Env, error) {
	if bool(args.IsNull()) || !args.HasNext() {
		return nil, env, errors.New("wrong number of arguments")
	}

	condition, err := Eval(args.This, env)
	if err != nil {
		return nil, nil, err
	}

	if isTrue(condition) {
		return args.Next.This, env, nil
	} else {
		if !args.Next.HasNext() {
			return nil, env, nil
		}
		return args.Next.Next.This, env, nil
	}
}

// Evaluate all but last args, return last arg and enclosing environment
func partialEval(args *Pair, env *Env) (Any, *Env, error) {
	current := args
	for current.HasNext() {
		_, err := Eval(current.This, env)
		if err != nil {
			return nil, nil, err
		}
		current = current.Next
	}
	return current.This, env, nil
}
