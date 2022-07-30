package scheme

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `quote` procedure
func Quote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	return args.This, nil
}

// `sefine` procedure
func Define(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	switch this := args.This.(type) {
	case types.Symbol:
		val, err := eval.Eval(args.Next.This, env)
		if err != nil {
			return nil, err
		}
		env.Set(this, val)
		return val, nil
	case *types.Pair:
		return defineLambda(this, args.Next, env)
	default:
		return nil, eval.NewErrBadName(args.This)
	}
}

// Implementation of
//
//  (define (name args...) body...)
func defineLambda(args, body *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	name, ok := args.This.(types.Symbol)
	if !ok {
		return nil, eval.NewErrBadName(args.This)
	}
	vars, err := eval.SymbolsPairToSlice(args.Next)
	if err != nil {
		return nil, err
	}
	fn := eval.Lambda{Vars: vars, Body: body, ParentEnv: env}
	env.Set(name, fn)
	return fn, nil
}

// `set!` procedure
func Set(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	val, err := eval.Eval(args.Next.This, env)
	if err != nil {
		return nil, err
	}

	switch name := args.This.(type) {
	case types.Symbol:
		if localEnv, ok := env.FindEnv(name); ok {
			localEnv.Set(name, val)
		} else {
			env.Set(name, val)
		}
		return val, nil
	default:
		return nil, eval.NewErrBadName(args.This)
	}
}

// `load` procedure
func Load(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	head, err := eval.Eval(args.This, env)
	if err != nil {
		return nil, err
	}
	path, ok := head.(types.String)
	if !ok {
		return nil, fmt.Errorf("invalid path: %v", head)
	}
	sexprs, err := eval.LoadEval(string(path), env)
	if err != nil {
		return nil, err
	}
	if len(sexprs) > 0 {
		return sexprs[len(sexprs)-1], nil
	}
	return nil, nil
}

// `eval` procedure
func Eval(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	// this is what "just" evaluating the object would do
	expr, err := eval.Eval(args.This, env)
	if err != nil {
		return nil, err
	}
	// here we evaluate the resulting expression
	return eval.Eval(expr, env)
}

func isCallable(obj types.Sexpr) bool {
	switch obj.(type) {
	case eval.Procedure, eval.Primitive, eval.TailCallOptimized, eval.Lambda:
		return true
	default:
		return false
	}
}
