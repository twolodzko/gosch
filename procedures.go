package main

import (
	"errors"
	"fmt"
)

func procedure(name string) (func(*Pair) (Any, error), bool) {
	switch name {
	case "car":
		return car, true
	case "cdr":
		return cdr, true
	case "null?":
		return isNull, true
	case "pair?":
		return isPair, true
	case "cons":
		return cons, true
	case "list":
		return list, true
	case "not":
		return not, true
	case "eq?":
		return eq, true
	case "and":
		return and, true
	case "or":
		return or, true
	case "+":
		return sum, true
	case "-":
		return dif, true
	case "*":
		return mul, true
	case "/":
		return div, true
	case "modulo":
		return mod, true
	case "=":
		return equal, true
	case "<":
		return lower, true
	case ">":
		return higher, true
	default:
		return nil, false
	}
}

func car(args *Pair) (Any, error) {
	switch val := args.This.(type) {
	case *Pair:
		return val.This, nil
	default:
		return nil, fmt.Errorf("%v is not a Pair", args.This)
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
		return nil, fmt.Errorf("%v is not a Pair", args.This)
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
