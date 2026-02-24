package special_forms

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.Procedure = Not
var _ eval.Procedure = And
var _ eval.Procedure = Or

func Not(args any, env *envir.Env) (any, error) {
	if args == nil {
		return false, nil
	}
	v, err := eval.EvalOne(args, env)
	return !types.IsTrue(v), err
}

func And(args any, env *envir.Env) (any, error) {
	var (
		this any
		ok   bool
	)
	for args != nil {
		this, args, ok = unpack(args)
		if !ok {
			return nil, eval.ErrSyntax
		}
		test, err := eval.Eval(this, env)
		if err != nil {
			return nil, err
		}
		if !types.IsTrue(test) {
			return false, nil
		}
	}
	return true, nil
}

func Or(args any, env *envir.Env) (any, error) {
	var (
		this any
		ok   bool
	)
	for args != nil {
		this, args, ok = unpack(args)
		if !ok {
			return nil, eval.ErrSyntax
		}
		test, err := eval.Eval(this, env)
		if err != nil {
			return nil, err
		}
		if types.IsTrue(test) {
			return true, nil
		}
	}
	return false, nil
}
