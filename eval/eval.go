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
		case *types.Pair:
			if val.IsNull() {
				return &types.Pair{}, nil
			}

			name := val.This
			args := val.Next

			callable, err := Eval(name, env)
			if err != nil {
				return nil, err
			}

			switch fn := callable.(type) {
			case Primitive:
				args, _, err := EvalEach(args, env)
				if err != nil {
					return nil, err
				}
				sexpr, err := fn(args)
				return sexpr, NewTraceback(name, err)
			case Procedure:
				sexpr, err := fn(args, env)
				return sexpr, NewTraceback(name, err)
			case TailCallOptimized:
				sexpr, env, err = fn(args, env)
				if err != nil {
					return nil, NewTraceback(name, err)
				}
			case Callable:
				sexpr, env, err = fn.Call(args, env)
				if err != nil {
					return nil, NewTraceback(name, err)
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
		if val, ok := env.Get(val); ok {
			return val, nil
		}
		return nil, fmt.Errorf("unbound variable %v", val)
	default:
		return val, nil
	}
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

// Evaluate all expressions, return all the results and the last result
func EvalEach(exprs *types.Pair, env *envir.Env) (*types.Pair, types.Sexpr, error) {
	if exprs == nil {
		return nil, nil, nil
	}
	var (
		vals   = types.NewAppendablePair()
		result types.Sexpr
		err    error
	)
	head := exprs
	for head != nil {
		result, err = Eval(head.This, env)
		if err != nil {
			return nil, nil, err
		}
		vals.Append(result)
		head = head.Next
	}
	return vals.ToPair(), result, err
}
