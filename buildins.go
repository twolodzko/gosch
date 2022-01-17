package main

import "fmt"

func car(pair *Pair) (Sexpr, error) {
	switch val := pair.This.Value.(type) {
	case *Pair:
		return val.This, nil
	default:
		return Sexpr{}, fmt.Errorf("%v is not a Pair", pair.This)
	}
}

func cdr(pair *Pair) (Sexpr, error) {
	switch val := pair.This.Value.(type) {
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
		return Sexpr{}, fmt.Errorf("%v is not a Pair", pair.This)
	}
}

func isNull(pair *Pair) (Sexpr, error) {
	if val, ok := pair.This.Value.(*Pair); ok {
		return Sexpr{val.IsNull(), false}, nil
	}
	return Sexpr{false, false}, nil
}

func isPair(pair *Pair) (Sexpr, error) {
	if val, ok := pair.This.Value.(*Pair); ok {
		return Sexpr{!val.IsNull(), false}, nil
	}
	return Sexpr{false, false}, nil
}
