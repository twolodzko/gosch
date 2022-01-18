package main

import (
	"errors"
	"fmt"
)

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
