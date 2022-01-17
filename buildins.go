package main

import "fmt"

func car(sexpr Sexpr) (Sexpr, error) {
	switch val := sexpr.Value.(type) {
	case *Pair:
		return val.This, nil
	default:
		return Sexpr{}, fmt.Errorf("%v is not a Pair", sexpr)
	}
}

func cdr(sexpr Sexpr) (Sexpr, error) {
	switch val := sexpr.Value.(type) {
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
		return Sexpr{}, fmt.Errorf("%v is not a Pair", sexpr)
	}
}

func isNull(sexpr Sexpr) (Sexpr, error) {
	if val, ok := sexpr.Value.(*Pair); ok {
		return Sexpr{val.IsNull(), false}, nil
	}
	return Sexpr{false, false}, nil
}

func isPair(sexpr Sexpr) (Sexpr, error) {
	if val, ok := sexpr.Value.(*Pair); ok {
		return Sexpr{!val.IsNull(), false}, nil
	}
	return Sexpr{false, false}, nil
}
