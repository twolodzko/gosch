package special_forms

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.TailCallOpt = Let
var _ eval.TailCallOpt = LetStar

// `let` procedure
//
//	(let ((key1 value1) (key2 value2) ...) expr1 expr2 ...)
func Let(args any, env *envir.Env) (any, *envir.Env, error) {
	this, tail, ok := unpack(args)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	switch name := this.(type) {
	case types.Symbol:
		return namedLet(name, tail, env)
	default:
		body, ok := tail.(types.Pair)
		if !ok {
			return nil, nil, eval.ErrSyntax
		}
		return regularLet(this, body, env)
	}
}

func regularLet(bindings any, body types.Pair, env *envir.Env) (any, *envir.Env, error) {
	local := envir.NewEnvFrom(env)
	switch b := bindings.(type) {
	case nil:
	case types.Pair:
		if err := setBindings(b, local, env); err != nil {
			return nil, local, err
		}
	default:
		return nil, nil, eval.NonList{Val: bindings}
	}
	return eval.PartialEval(body, local)
}

func namedLet(name types.Symbol, rest any, env *envir.Env) (any, *envir.Env, error) {
	this, tail, ok := unpack(rest)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}

	bindings, ok := this.(types.Pair)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	body, ok := tail.(types.Pair)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	var (
		keys, vals []any
		head       any = bindings
	)
	for head != nil {
		p, ok := head.(types.Pair)
		if !ok {
			return nil, nil, eval.ErrSyntax
		}
		b, ok := p.This.(types.Pair)
		if !ok {
			return nil, nil, eval.ErrSyntax
		}
		name, arg, err := extractBinding(b)
		if err != nil {
			return nil, env, err
		}
		keys = append(keys, name)
		vals = append(vals, arg)
		head = p.Next
	}

	k := types.List(keys...)
	v := types.List(vals...)

	local := envir.NewEnvFrom(env)
	lambda := Lambda{k, body, local}
	local.Set(name, lambda)

	return lambda.Call(v, local)
}

// `let*` procedure
//
//	(let* ((name1 value1) (name2 value2) ...) expr1 expr2 ...)
func LetStar(args any, env *envir.Env) (any, *envir.Env, error) {
	this, body, ok := unpack(args)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	local := envir.NewEnvFrom(env)
	b, ok := this.(types.Pair)
	if !ok {
		return nil, nil, eval.ErrSyntax
	}
	if err := setBindings(b, local, local); err != nil {
		return nil, nil, err
	}
	return eval.PartialEval(body, local)
}

// Iterate through the bindings ((key1 value1) (key2 value2) ...) and set them to an environment
func setBindings(bindings types.Pair, local, parent *envir.Env) error {
	return bindings.TryForEach(func(val any) error {
		p, ok := val.(types.Pair)
		if !ok {
			return eval.NonList{Val: val}
		}
		return bind(p, local, parent)
	})
}

// Bind value to the name in the local env
func bind(binding types.Pair, local, parent *envir.Env) error {
	name, expr, err := extractBinding(binding)
	if err != nil {
		return err
	}
	// arguments are evaluated in env enclosing let
	val, err := eval.Eval(expr, parent)
	if err != nil {
		return err
	}
	local.Set(name, val)
	return nil
}

// Extract name and value for the binding
func extractBinding(arg types.Pair) (types.Symbol, any, error) {
	switch name := arg.This.(type) {
	case types.Symbol:
		p, ok := arg.Next.(types.Pair)
		if !ok {
			return "", nil, eval.ErrSyntax
		}
		return name, p.This, nil
	default:
		return "", nil, eval.InvalidName{Val: arg.This}
	}
}
