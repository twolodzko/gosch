package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

var DEBUG bool = false

func Eval(sexpr types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	for {
		if DEBUG {
			fmt.Printf(" ↪ Step: %v\n", sexpr)
			fmt.Printf("   Env:  %v\n", env)
		}

		switch val := sexpr.(type) {
		case types.Symbol:
			return getSymbol(val, env)
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
			case Lambda:
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
		if fn, ok := procedure(val); ok {
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
	var (
		head *types.Pair
		args []types.Sexpr
	)
	head = pair
	for head != nil {
		sexpr, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
		head = head.Next
	}
	// TODO: avoid re-packing
	return types.PairFromArray(args), nil
}
