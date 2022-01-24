package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

var DEBUG bool = false

func Eval(sexpr types.Any, env *envir.Env) (types.Any, error) {
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
			case TcoProcedure:
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

func getSymbol(sexpr types.Any, env *envir.Env) (types.Any, error) {
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

// Evaluate all but last args, return last arg and enclosing environment
func partialEval(args *types.Pair, env *envir.Env) (types.Any, *envir.Env, error) {
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
