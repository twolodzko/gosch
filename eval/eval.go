package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func EvalString(code string, env *envir.Env) ([]types.Any, *envir.Env, error) {
	var out []types.Any
	parser := parser.NewParser(code)
	sexprs, err := parser.Read()
	if err != nil {
		return nil, env, err
	}
	for _, sexpr := range sexprs {
		result, err := Eval(sexpr, env)
		if err != nil {
			return nil, env, err
		}
		out = append(out, result)
	}
	return out, env, err
}

func Eval(sexpr types.Any, env *envir.Env) (types.Any, error) {
	var err error
	for {
		// fmt.Printf("Evaluating: %v :: %T\n", sexpr, sexpr)
		switch val := sexpr.(type) {
		case string:
			sexpr, err = getSymbolValue(val, env)
			if err != nil {
				return nil, err
			}
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

func getSymbolValue(name string, env *envir.Env) (types.Any, error) {
	if fn, ok := procedure(name); ok {
		return fn, nil
	}
	return env.Get(name)
}
