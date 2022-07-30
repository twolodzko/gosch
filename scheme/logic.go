package scheme

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

const Else = types.Bool(true)

func Not(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return types.Bool(false), nil
	}
	return !types.IsTrue(args.This), nil
}

func And(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		test, err := eval.Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if !types.IsTrue(test) {
			return types.Bool(false), nil
		}
		head = head.Next
	}
	return types.Bool(true), nil
}

func Or(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		test, err := eval.Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if types.IsTrue(test) {
			return types.Bool(true), nil
		}
		head = head.Next
	}
	return types.Bool(false), nil
}
