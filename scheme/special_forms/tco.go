package special_forms

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.TailCallOptimized = If
var _ eval.TailCallOptimized = Cond
var _ eval.TailCallOptimized = Begin

// `if` procedure
//
//	(if condition if-true if-false)
func If(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil || !args.HasNext() {
		return nil, env, eval.ErrBadArgNumber
	}

	condition, err := eval.Eval(args.This, env)
	if err != nil {
		return nil, nil, err
	}

	if types.IsTrue(condition) {
		return args.Next.This, env, nil
	} else {
		if !args.Next.HasNext() {
			return nil, env, nil
		}
		return args.Next.Next.This, env, nil
	}
}

// `cond` procedure
//
//	(cond (test1 expr1) (test2 expr2)...)
func Cond(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil {
		return nil, env, eval.ErrBadArgNumber
	}

	head := args
	for head != nil {
		switch pair := head.This.(type) {
		case *types.Pair:
			if pair.IsNull() || !pair.HasNext() || pair.Next.HasNext() {
				return nil, env, fmt.Errorf("invalid argument %v", pair)
			}
			condition, err := eval.Eval(pair.This, env)
			if err != nil {
				return nil, env, err
			}
			if types.IsTrue(condition) {
				return pair.Next.This, env, nil
			}
		default:
			return nil, env, fmt.Errorf("invalid argument %v", head.This)
		}
		head = head.Next
	}
	return nil, env, nil
}

// `begin` procedure
func Begin(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	return eval.PartialEval(args, env)
}
