package scheme

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func Not(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return types.FALSE, nil
	}
	return !types.IsTrue(args.This), nil
}

func And(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.This == nil {
		return types.TRUE, nil
	}
	head := args
	for head != nil {
		test, err := eval.Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if !types.IsTrue(test) {
			return types.FALSE, nil
		}
		head = head.Next
	}
	return types.TRUE, nil
}

func Or(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.This == nil {
		return types.TRUE, nil
	}
	head := args
	for head != nil {
		test, err := eval.Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if types.IsTrue(test) {
			return types.TRUE, nil
		}
		head = head.Next
	}
	return types.FALSE, nil
}
