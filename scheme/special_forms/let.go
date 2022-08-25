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

	local := envir.NewEnvFrom(env)

	// bind variables
	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return nil, local, eval.NewErrNonList(args.This)
	}
	err := setBindings(bindings, local, env)
	if err != nil {
		return nil, local, err
	}

	return eval.PartialEval(args.Next, local)
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
	if name, ok := binding.This.(types.Symbol); ok {
		if !binding.HasNext() {
			return fmt.Errorf("%v has not value to bind", binding)
		}
		// arguments are evaluated in env enclosing let
		val, err := eval.Eval(binding.Next.This, parent)
		if err != nil {
			return err
		}
		local.Set(name, val)
		return nil
	}
	return fmt.Errorf("binding %v does not use proper name", binding)
}
