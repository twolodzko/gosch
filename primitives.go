package main

import (
	"errors"
	"fmt"
)

func car(args *Pair) (Any, error) {
	switch val := args.This.(type) {
	case *Pair:
		return val.This, nil
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func cdr(args *Pair) (Any, error) {
	switch val := args.This.(type) {
	case *Pair:
		switch {
		case val.Next == nil:
			return nil, nil
		case val.Next.Next == nil:
			return val.Next.This, nil
		default:
			return val.Next, nil
		}
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func isNull(args *Pair) (Any, error) {
	switch val := args.This.(type) {
	case *Pair:
		return val.IsNull(), nil
	default:
		return Bool(false), nil
	}
}

func isPair(args *Pair) (Any, error) {
	switch val := args.This.(type) {
	case *Pair:
		return !val.IsNull(), nil
	default:
		return Bool(false), nil
	}
}

func cons(args *Pair) (Any, error) {
	if !args.HasNext() || args.Next.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.Next.This.(type) {
	case *Pair:
		return val.Cons(args.This), nil
	default:
		return list(args)
	}
}

func list(args *Pair) (Any, error) {
	return args, nil
}

func not(args *Pair) (Any, error) {
	return !isTrue(args.This), nil
}

func eq(args *Pair) (Any, error) {
	if !args.HasNext() || args.Next.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	return Bool(args.This == args.Next.This), nil
}

func and(args *Pair) (Any, error) {
	if args.This == nil {
		return Bool(true), nil
	}
	head := args
	for head != nil {
		if !isTrue(head.This) {
			return Bool(false), nil
		}
		head = head.Next
	}
	return Bool(true), nil
}

func or(args *Pair) (Any, error) {
	if args.This == nil {
		return Bool(true), nil
	}
	head := args
	for head != nil {
		if isTrue(head.This) {
			return Bool(true), nil
		}
		head = head.Next
	}
	return Bool(false), nil
}

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

func set(args *Pair, env *Env) (Any, error) {
	val, err := Eval(args.Next.This, env)
	if err != nil {
		return nil, err
	}

	switch name := args.This.(type) {
	case string:
		if localEnv, ok := env.FindEnv(name); ok {
			localEnv.Set(name, val)
		} else {
			env.Set(name, val)
		}
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}
