package special_forms

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.TailCallOptimized = Let
var _ eval.TailCallOptimized = LetStar

// Tail call optimized procedures
// see: https://github.com/kanaka/mal/blob/master/process/guide.md#step-5-tail-call-optimization

// `let` procedure
//
//	(let ((name1 value1) (name2 value2) ...) expr1 expr2 ...)
func Let(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil || !args.HasNext() {
		return nil, env, eval.ErrBadArgNumber
	}

	switch first := args.This.(type) {
	case *types.Pair:
		return regularLet(first, args.Next, env)
	case types.Symbol:
		return namedLet(first, args.Next, env)
	default:
		return nil, env, eval.ErrInvalidSyntax
	}
}

func regularLet(bindings, body *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	local := envir.NewEnvFrom(env)
	if err := setBindings(bindings, local, env); err != nil {
		return nil, local, err
	}
	return eval.PartialEval(body, local)
}

func namedLet(name types.Symbol, rest *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if !rest.HasNext() {
		return nil, env, eval.ErrBadArgNumber
	}
	head, ok := rest.This.(*types.Pair)
	if !ok {
		return nil, env, eval.NewErrNonList(rest.This)
	}
	var (
		vars []types.Symbol
		args = types.NewAppendablePair()
	)
	for head != nil {
		switch binding := head.This.(type) {
		case *types.Pair:
			name, arg, err := extractBinding(binding)
			if err != nil {
				return nil, env, err
			}
			vars = append(vars, name)
			args.Append(arg)
		default:
			return nil, env, eval.NewErrNonList(binding)
		}
		head = head.Next
	}

	local := envir.NewEnvFrom(env)
	lambda := Lambda{vars, rest.Next, local}
	local.Set(name, lambda)

	return lambda.Call(args.ToPair(), local)
}

// `let*` procedure
//
//	(let* ((name1 value1) (name2 value2) ...) expr1 expr2 ...)
func LetStar(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil || !args.HasNext() {
		return nil, env, eval.ErrBadArgNumber
	}

	local := envir.NewEnvFrom(env)

	// bind variables
	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return nil, local, eval.NewErrNonList(args.This)
	}
	err := setBindings(bindings, local, local)
	if err != nil {
		return nil, local, err
	}

	return eval.PartialEval(args.Next, local)
}

// Iterate through the bindings ((name1 value1) (name2 value2) ...) and set them to an environment
func setBindings(bindings *types.Pair, local, parent *envir.Env) error {
	if bindings.IsNull() {
		return nil
	}

	head := bindings
	for head != nil {
		switch pair := head.This.(type) {
		case *types.Pair:
			err := bind(pair, local, parent)
			if err != nil {
				return err
			}
		default:
			return eval.NewErrNonList(head.This)
		}
		head = head.Next
	}
	return nil
}

func bind(binding *types.Pair, local, parent *envir.Env) error {
	name, sexpr, err := extractBinding(binding)
	if err != nil {
		return err
	}
	// arguments are evaluated in env enclosing let
	val, err := eval.Eval(sexpr, parent)
	if err != nil {
		return err
	}
	local.Set(name, val)
	return nil
}

func extractBinding(arg *types.Pair) (types.Symbol, types.Sexpr, error) {
	if !arg.HasNext() {
		return "", nil, fmt.Errorf("%v has not value to bind", arg)
	}
	switch name := arg.This.(type) {
	case types.Symbol:
		return name, arg.Next.This, nil
	default:
		return "", nil, eval.NewErrBadName(arg.This)
	}
}
