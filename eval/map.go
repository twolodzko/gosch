package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

// `map` procedure
func mapFn(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	fn, list, err := mapArgs(args, env)
	if err != nil {
		return nil, err
	}
	if list.IsNull() {
		return &types.Pair{}, nil
	}

	ac := types.NewAppendablePair()
	for list != nil {
		pair := types.NewPair(fn, list.This)
		result, err := Eval(pair, env)
		if err != nil {
			return nil, err
		}
		ac.Append(result)

		list = list.Next
	}
	return ac.ToPair(), nil
}

// `go` procedure: a parallel map
//
//  (go fn lst)
//
// Limitations:
//  * it ignores errors in the routines and just returns <nil>
//  * it can panic for impure functions (e.g. using set!)
func goFn(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	fn, list, err := mapArgs(args, env)
	if err != nil {
		return nil, err
	}
	if list.IsNull() {
		return &types.Pair{}, nil
	}

	size := list.Len()
	ch := make(chan types.Sexpr)

	job := func(arg types.Sexpr) {
		pair := types.NewPair(fn, arg)
		result, err := Eval(pair, env)
		if err != nil {
			ch <- nil
		} else {
			ch <- result
		}
	}

	for list != nil {
		go job(list.This)

		list = list.Next
	}

	results := types.NewAppendablePair()
	for i := 0; i < size; i++ {
		results.Append(<-ch)
	}
	return results.ToPair(), nil
}

func mapArgs(args *types.Pair, env *envir.Env) (types.Sexpr, *types.Pair, error) {
	if args == nil || !args.HasNext() {
		return nil, nil, ErrBadArgNumber
	}
	fn, err := Eval(args.This, env)
	if err != nil {
		return nil, nil, err
	}
	list, err := evalToList(args.Next.This, env)
	if err != nil {
		return nil, nil, err
	}
	return fn, list, nil
}

func evalToList(sexpr types.Sexpr, env *envir.Env) (*types.Pair, error) {
	val, err := Eval(sexpr, env)
	if err != nil {
		return nil, err
	}
	list, ok := val.(*types.Pair)
	if !ok {
		return nil, fmt.Errorf("%v is not a list", val)
	}
	return list, nil
}
