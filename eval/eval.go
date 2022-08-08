package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

func Eval(sexpr types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	for {
		if DEBUG {
			fmt.Printf(" ↪ Step: %v\n", sexpr)
			fmt.Printf("   Env:  %v\n", env)
		}

		switch val := sexpr.(type) {
		case types.Symbol:
			return getSymbol(val, env)
		case Getable:
			return val.Get()
		case *types.Pair:
			if val.IsNull() {
				return &types.Pair{}, nil
			}

			callable, err := Eval(val.This, env)
			if err != nil {
				return nil, err
			}

			switch fn := callable.(type) {
			case Primitive:
				args, err := evalArgs(val.Next, env)
				if err != nil {
					return nil, err
				}
				return fn(args)
			case Procedure:
				return fn(val.Next, env)
			case TailCallOptimized:
				sexpr, env, err = fn(val.Next, env)
				if err != nil {
					return nil, err
				}
			case Callable:
				sexpr, env, err = fn.Call(val.Next, env)
				if err != nil {
					return nil, err
				}
			default:
				return nil, fmt.Errorf("%v is not callable", fn)
			}
		default:
			return sexpr, nil
		}
	}
}

func getSymbol(sexpr types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	switch val := sexpr.(type) {
	case types.Symbol:
		if fn, ok := Procedures[val]; ok {
			return fn, nil
		}
		return env.Get(val)
	default:
		return val, nil
	}
}

func evalArgs(pair *types.Pair, env *envir.Env) (*types.Pair, error) {
	if pair == nil {
		return nil, nil
	}
	var head *types.Pair
	args := types.NewAppendablePair()
	head = pair
	for head != nil {
		sexpr, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		args.Append(sexpr)
		head = head.Next
	}
	return args.ToPair(), nil
}

// Evaluate all args but last, return the last arg and the enclosing environment
func PartialEval(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil {
		return nil, env, nil
	}
	current := args
	for current.HasNext() {
		_, err := Eval(current.This, env)
		if err != nil {
			return nil, nil, err
		}
		current = current.Next
	}
	return current.This, env, nil
}

// Evaluate all expressions, return the last result
func EvalAll(exprs *types.Pair, env *envir.Env) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	head := exprs
	for head != nil {
		result, err = Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		head = head.Next
	}
	return result, nil
}
