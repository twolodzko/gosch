package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

// Tail call optimized procedures
// see: https://github.com/kanaka/mal/blob/master/process/guide.md#step-5-tail-call-optimization

// Evaluate all but last args, return last arg and enclosing environment
func partialEval(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil {
		return nil, env, nil
	}
	current := args
	for current.HasNext() {
		_, err := Eval(current.This, env)
		if err != nil {
			return nil, nil, err
		}
		current = current.Next
	}
	return current.This, env, nil
}

func let(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil || !args.HasNext() {
		return nil, env, ErrBadArgNumber
	}

	local := envir.NewEnv()
	local.Parent = env

	// bind variables
	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return nil, local, &ErrNonList{args.This}
	}
	err := setBindings(bindings, local, env)
	if err != nil {
		return nil, local, err
	}

	return partialEval(args.Next, local)
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
			return &ErrNonList{head.This}
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
		val, err := Eval(binding.Next.This, parent)
		if err != nil {
			return err
		}
		local.Set(name, val)
		return nil
	}
	return fmt.Errorf("binding %v does not use proper name", binding)
}

func ifFn(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil || !args.HasNext() {
		return nil, env, ErrBadArgNumber
	}

	condition, err := Eval(args.This, env)
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

func cond(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	if args == nil {
		return nil, env, ErrBadArgNumber
	}

	head := args
	for head != nil {
		switch pair := head.This.(type) {
		case *types.Pair:
			if pair.IsNull() || !pair.HasNext() || pair.Next.HasNext() {
				return nil, env, fmt.Errorf("invalid argument %v", pair)
			}
			condition, err := Eval(pair.This, env)
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
