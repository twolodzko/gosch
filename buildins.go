package main

import (
	"errors"
	"fmt"

	"github.com/google/go-cmp/cmp"
)

func buildin(name string) (func(*Pair) (Any, error), bool) {
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
	case "eq?":
		return eq, true
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
	return !IsTrue(args.This), nil
}

func eq(args *Pair) (Any, error) {
	if !args.HasNext() || args.Next.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	return Bool(cmp.Equal(args.This, args.Next.This)), nil
}
