package primitives

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.Procedure = Car
var _ eval.Procedure = Cdr
var _ eval.Procedure = Cons

func Car(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, eval.SyntaxError
	}
	v, err := eval.Eval(p.This, env)
	if err != nil {
		return nil, err
	}

	switch p := v.(type) {
	case types.Pair:
		return p.This, nil
	default:
		return nil, eval.NonList{Val: v}
	}
}

func Cdr(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, eval.SyntaxError
	}
	v, err := eval.Eval(p.This, env)
	if err != nil {
		return nil, err
	}
	switch p := v.(type) {
	case types.Pair:
		return p.Next, nil
	default:
		return nil, eval.NonList{Val: v}
	}
}

func Cons(args any, env *envir.Env) (any, error) {
	a, b, err := eval.EvalTwo(args, env)
	if err != nil {
		return nil, err
	}
	return types.Cons(a, b), nil
}

func List(args any, env *envir.Env) (any, error) {
	if args == nil {
		return nil, nil
	}
	vals, err := eval.ListMapEval(args, env)
	return types.List(vals...), err
}
