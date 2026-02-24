package primitives

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `null?` procedure
func IsNull(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	return v == nil, err
}

// `pair?` procedure
func IsPair(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	_, ok := v.(types.Pair)
	return ok, err
}

// `bool?` procedure
func IsBool(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	_, ok := v.(bool)
	return ok, err
}

// `symbol?` procedure
func IsSymbol(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	_, ok := v.(types.Symbol)
	return ok, err
}

// `procedure?` procedure
func IsProcedure(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	return isCallable(v), err
}

func isCallable(obj any) bool {
	switch obj.(type) {
	case eval.Procedure, eval.TailCallOpt, eval.Callable:
		return true
	default:
		return false
	}
}
