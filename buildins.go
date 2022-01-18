package main

import (
	"errors"
	"fmt"
)

func buildin(name string) (func(*Pair) (Sexpr, error), bool) {
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
	default:
		return nil, false
	}
}

func car(args *Pair) (Sexpr, error) {
	switch val := args.This.Value.(type) {
	case *Pair:
		return val.This, nil
	default:
		return Sexpr{}, fmt.Errorf("%v is not a Pair", args.This)
	}
}

func cdr(args *Pair) (Sexpr, error) {
	switch val := args.This.Value.(type) {
	case *Pair:
		switch {
		case val.IsNull():
			return Sexpr{}, nil
		case val.Next == nil:
			return Sexpr{&Pair{}, false}, nil
		default:
			return Sexpr{val.Next, false}, nil
		}
	default:
		return Sexpr{}, fmt.Errorf("%v is not a Pair", args.This)
	}
}

func isNull(args *Pair) (Sexpr, error) {
	switch val := args.This.Value.(type) {
	case *Pair:
		return Sexpr{val.IsNull(), false}, nil
	default:
		return Sexpr{false, false}, nil
	}
}

func isPair(args *Pair) (Sexpr, error) {
	switch val := args.This.Value.(type) {
	case *Pair:
		return Sexpr{!val.IsNull(), false}, nil
	default:
		return Sexpr{false, false}, nil
	}
}

func cons(args *Pair) (Sexpr, error) {
	if !args.HasNext() || args.Next.HasNext() {
		return Sexpr{}, errors.New("wrong number of arguments")
	}
	switch val := args.Next.This.Value.(type) {
	case *Pair:
		return Sexpr{val.Cons(args.This), false}, nil
	default:
		return list(args)
	}
}

func list(args *Pair) (Sexpr, error) {
	return Sexpr{args, false}, nil
}

func (env *Env) Define(args *Pair) (Sexpr, error) {
	switch name := args.This.Value.(type) {
	case string:
		val, err := env.Eval(args.Next.This)
		if err != nil {
			return Sexpr{}, err
		}
		env.Set(name, val)
		return val, nil
	default:
		return Sexpr{}, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}
