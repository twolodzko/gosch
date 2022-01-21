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
	err := local.setBindings(bindings)
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
func (e *Env) setBindings(bindings *Pair) error {
	head := bindings
	for head != nil {
		binding, ok := head.This.(*Pair)
		if !ok {
			return fmt.Errorf("%v is not a list", head.This)
		}
		// unpack binding (name value)
		if name, ok := binding.This.(string); ok {
			if !binding.HasNext() {
				return fmt.Errorf("%v has not value to bind", binding)
			}
			e.Set(name, binding.Next.This)
		} else {
			return fmt.Errorf("binding %v does not use proper name", binding)
		}
		head = head.Next
	}
	return nil
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

// func lambda(args *Pair, env *Env) (Any, error) {

// }
