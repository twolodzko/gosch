package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

func Eval(sexpr any, env *envir.Env) (any, error) {
	for {
		if Debug {
			fmt.Printf(" ↪ eval:  %v\n", types.ToString(sexpr))
			fmt.Printf("   env:   %v\n", env)
		}

		switch val := sexpr.(type) {
		case types.Symbol:
			return getSymbol(val, env)
		case types.Pair:
			name := val.This
			args := val.Next

			callable, err := Eval(name, env)
			if err != nil {
				return nil, err
			}

			switch fn := callable.(type) {
			case Procedure:
				val, err := fn(args, env)
				return val, NewTraceback(name, err)
			case TailCallOpt:
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
				return nil, fmt.Errorf("%v is not callable", types.ToString(fn))
			}
		default:
			return sexpr, nil
		}
	}
}

func getSymbol(sexpr any, env *envir.Env) (any, error) {
	switch val := sexpr.(type) {
	case types.Symbol:
		if val, ok := env.Get(val); ok {
			return val, nil
		}
		return nil, fmt.Errorf("unbound variable %v", val)
	default:
		return val, nil
	}
}

// Evaluate all args but last, return the last arg and the enclosing environment
func PartialEval(args any, env *envir.Env) (any, *envir.Env, error) {
	head := args
	for {
		switch p := head.(type) {
		case types.Pair:
			if p.Next == nil {
				return p.This, env, nil
			}
			if _, err := Eval(p.This, env); err != nil {
				return nil, nil, err
			}
			head = p.Next
		default:
			return head, env, nil
		}
	}
}

func EvalOne(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, SyntaxError
	}
	if p.Next != nil {
		return nil, ArityError
	}
	return Eval(p.This, env)
}

// Evaluate two expressions
func EvalTwo(args any, env *envir.Env) (any, any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, nil, SyntaxError
	}
	a, err := Eval(p.This, env)
	if err != nil {
		return nil, nil, err
	}
	p, ok = p.Next.(types.Pair)
	if !ok || p.Next != nil {
		return nil, nil, ArityError
	}
	b, err := Eval(p.This, env)
	return a, b, err
}

func ListMapEval(list any, env *envir.Env) ([]any, error) {
	var (
		head any = list
		acc  []any
	)
	for head != nil {
		p, ok := head.(types.Pair)
		if !ok {
			return nil, NonList{list}
		}
		res, err := Eval(p.This, env)
		if err != nil {
			return nil, err
		}
		acc = append(acc, res)
		head = p.Next
	}
	return acc, nil
}
