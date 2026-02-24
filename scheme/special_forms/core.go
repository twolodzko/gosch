package special_forms

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `define` procedure
func Define(args any, env *envir.Env) (any, error) {
	head, body, ok := unpack(args)
	if !ok {
		return nil, eval.ErrArity
	}

	switch this := head.(type) {
	case types.Symbol:
		body, tail, ok := unpack(body)
		if !ok || tail != nil {
			return nil, eval.ErrSyntax
		}
		val, err := eval.Eval(body, env)
		if err != nil {
			return nil, err
		}
		env.Set(this, val)
		return val, nil
	case types.Pair:
		body, ok := body.(types.Pair)
		if !ok {
			return nil, eval.ErrSyntax
		}
		return defineLambda(this, body, env)
	default:
		return nil, eval.InvalidName{Val: this}
	}
}

// Implementation of
//
//	(define (name args...) body...)
func defineLambda(head, body types.Pair, env *envir.Env) (any, error) {
	name, ok := head.This.(types.Symbol)
	if !ok {
		return nil, eval.InvalidName{Val: head.This}
	}
	args := head.Next
	lambda := Lambda{args, body, env}
	env.Set(name, lambda)
	return lambda, nil
}

// `set!` procedure
func Set(args any, env *envir.Env) (any, error) {
	head, args, ok := unpack(args)
	if !ok {
		return nil, eval.ErrArity
	}
	name, ok := head.(types.Symbol)
	if !ok {
		return nil, eval.InvalidName{Val: head}
	}
	expr, tail, ok := unpack(args)
	if !ok || tail != nil {
		return nil, eval.ErrArity
	}
	val, err := eval.Eval(expr, env)
	if err != nil {
		return nil, err
	}
	if localEnv, ok := env.FindEnv(name); ok {
		localEnv.Set(name, val)
	} else {
		env.Set(name, val)
	}
	return val, nil
}

// `load` procedure
func Load(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	path, ok := v.(string)
	if !ok {
		return nil, fmt.Errorf("invalid path: %v", v)
	}
	sexprs, err := eval.LoadEval(path, env)
	if err != nil {
		return nil, err
	}
	if len(sexprs) > 0 {
		return sexprs[len(sexprs)-1], nil
	}
	return nil, nil
}

// `eval` procedure
func Eval(args any, env *envir.Env) (any, error) {
	val, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	// here we evaluate the resulting expression
	return eval.Eval(val, env)
}
