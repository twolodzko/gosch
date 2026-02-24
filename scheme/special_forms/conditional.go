package special_forms

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.TailCallOpt = If
var _ eval.TailCallOpt = Cond

// `if` procedure
//
//	(if condition if-true if-false)
func If(args any, env *envir.Env) (any, *envir.Env, error) {
	cond, tail, ok := unpack(args)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}

	condition, err := eval.Eval(cond, env)
	if err != nil {
		return nil, nil, err
	}
	yes, no, ok := unpack(tail)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	if types.IsTrue(condition) {
		return yes, env, nil
	} else {
		no, _, _ = unpack(no)
		return no, env, nil
	}
}

// `cond` procedure
//
//	(cond (test1 expr1) (test2 expr2)...)
func Cond(args any, env *envir.Env) (any, *envir.Env, error) {
	var (
		this any
		ok   bool
		head = args
	)
	for head != nil {
		this, head, ok = unpack(head)
		if !ok {
			return nil, nil, eval.ErrSyntax
		}
		cond, body, ok := unpack(this)
		if !ok {
			return nil, nil, eval.NonList{Val: this}
		}
		test, err := eval.Eval(cond, env)
		if err != nil {
			return nil, env, err
		}
		if types.IsTrue(test) {
			return eval.PartialEval(body, env)
		}
	}
	return nil, env, nil
}
