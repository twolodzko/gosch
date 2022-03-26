package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

// `go` procedure: a parallel map
//
//  (go fn lst)
//
// Limitations:
//  * it ignores errors in the routines and just returns <nil>
//  * it can panic for impure functions (e.g. using set!)
func goFn(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, ErrBadArgNumber
	}

	fn := args.This

	list, err := evalToList(args.Next.This, env)
	if err != nil || list == nil {
		return nil, err
	}

	ch := make(chan types.Sexpr)

	job := func(arg types.Sexpr) {
		pair := types.PairFromArray([]types.Sexpr{fn, arg})
		result, err := Eval(pair, env)
		if err != nil {
			ch <- nil
		} else {
			ch <- result
		}
	}

	var counter int
	for {
		counter++
		go job(list.This)

		list = list.Next
		if list == nil {
			break
		}
	}

	var results []types.Sexpr
	for i := 0; i < counter; i++ {
		results = append(results, <-ch)
	}

	return types.PairFromArray(results), nil
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
