package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

func let(args *types.Pair, env *envir.Env) (types.Any, *envir.Env, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, env, errors.New("wrong number of arguments")
	}

	local := envir.NewEnv()
	local.Parent = env

	// bind variables
	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return nil, local, fmt.Errorf("%v is not a list", args.This)
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
			return fmt.Errorf("%v is not a list", head.This)
		}
		head = head.Next
	}
	return nil
}

func bind(binding *types.Pair, local, parent *envir.Env) error {
	if name, ok := binding.This.(string); ok {
		if !binding.HasNext() {
			return fmt.Errorf("%v has not value to bind", binding)
		}
		val, err := Eval(binding.Next.This, parent)
		if err != nil {
			return err
		}
		local.Set(name, val)
		return nil
	}
	return fmt.Errorf("binding %v does not use proper name", binding)
}

func ifFn(args *types.Pair, env *envir.Env) (types.Any, *envir.Env, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, env, errors.New("wrong number of arguments")
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
