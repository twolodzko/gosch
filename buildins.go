package main

import (
	"errors"
	"fmt"
	"strconv"
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

func (env *Env) Define(args *Pair) (Any, error) {
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

func toInt(obj Any) (int, error) {
	switch x := obj.(type) {
	case int:
		return x, nil
	case string:
		number, err := strconv.Atoi(x)
		if err != nil {
			return 0, err
		}
		return number, nil
	default:
		return 0, fmt.Errorf("%v is not a number", obj)
	}
}

func sum(args *Pair) (Any, error) {
	var result int = 0
	if args.This == nil {
		return result, nil
	}
	head := args
	for head != nil {
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result += x
		head = head.Next
	}
	return result, nil
}

func dif(args *Pair) (Any, error) {
	if args.This == nil {
		return 0, nil
	}
	result, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	if !args.HasNext() {
		return -result, nil
	}
	head := args
	for head.HasNext() {
		head = head.Next
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result -= x
	}
	return result, nil
}

func mul(args *Pair) (Any, error) {
	var result int = 1
	if args.This == nil {
		return result, nil
	}
	head := args
	for head != nil {
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result *= x
		head = head.Next
	}
	return result, nil
}

func div(args *Pair) (Any, error) {
	if args.This == nil {
		return 1, nil
	}
	result, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	if !args.HasNext() {
		return 1 / result, nil
	}
	head := args
	for head.HasNext() {
		head = head.Next
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result /= x
	}
	return result, nil
}

func mod(args *Pair) (Any, error) {
	a, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	if !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	n, err := toInt(args.Next.This)
	if err != nil {
		return nil, err
	}
	return a % n, nil
}

func equal(args *Pair) (Any, error) {
	if args.This == nil || !args.HasNext() {
		return Bool(true), nil
	}
	prev, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	head := args
	for head.HasNext() {
		head = head.Next
		curr, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		if prev != curr {
			return Bool(false), nil
		}
	}
	return Bool(true), nil
}
