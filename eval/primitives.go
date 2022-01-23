package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

func car(args *types.Pair) (types.Any, error) {
	switch val := args.This.(type) {
	case *types.Pair:
		return val.This, nil
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func cdr(args *types.Pair) (types.Any, error) {
	switch val := args.This.(type) {
	case *types.Pair:
		if val.IsNull() {
			return nil, nil
		}
		switch {
		case val.Next == nil:
			return &types.Pair{}, nil
		default:
			return val.Next, nil
		}
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func cons(args *types.Pair) (types.Any, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.Next.This.(type) {
	case *types.Pair:
		return val.Cons(args.This), nil
	default:
		return list(args)
	}
}

func list(args *types.Pair) (types.Any, error) {
	return args, nil
}

func not(args *types.Pair) (types.Any, error) {
	return !types.IsTrue(args.This), nil
}

func eq(args *types.Pair) (types.Any, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	return types.Bool(args.This == args.Next.This), nil
}

func and(args *types.Pair) (types.Any, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		if !types.IsTrue(head.This) {
			return types.Bool(false), nil
		}
		head = head.Next
	}
	return types.Bool(true), nil
}

func or(args *types.Pair) (types.Any, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		if types.IsTrue(head.This) {
			return types.Bool(true), nil
		}
		head = head.Next
	}
	return types.Bool(false), nil
}

func define(args *types.Pair, env *envir.Env) (types.Any, error) {
	switch name := args.This.(type) {
	case types.Symbol:
		val, err := Eval(args.Next.This, env)
		if err != nil {
			return nil, err
		}
		env.Set(name, val)
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}

func isNull(args *types.Pair) (types.Any, error) {
	switch val := args.This.(type) {
	case *types.Pair:
		return val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

func isPair(args *types.Pair) (types.Any, error) {
	switch val := args.This.(type) {
	case *types.Pair:
		return !val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

func isNumber(args *types.Pair) (types.Any, error) {
	switch args.This.(type) {
	case int:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isBool(args *types.Pair) (types.Any, error) {
	switch args.This.(type) {
	case types.Bool:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isString(args *types.Pair) (types.Any, error) {
	switch args.This.(type) {
	case types.String:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isSymbol(args *types.Pair) (types.Any, error) {
	switch args.This.(type) {
	case types.Symbol:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isNil(args *types.Pair) (types.Any, error) {
	return types.Bool(args.This == nil), nil
}

func isProcedure(args *types.Pair) (types.Any, error) {
	switch args.This.(type) {
	case Procedure, Primitive, TcoProcedure, Lambda:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func toString(args *types.Pair, sep string) string {
	var out string
	head := args
	for head != nil {
		switch val := head.This.(type) {
		case types.String:
			out += fmt.Sprintf("%v", string(val))
		default:
			out += fmt.Sprintf("%v", val)
		}

		if head.HasNext() {
			out += sep
		}
		head = head.Next
	}
	return out
}

func display(args *types.Pair) (types.Any, error) {
	fmt.Printf("%s\n", toString(args, " "))
	return nil, nil
}

func raiseError(args *types.Pair) (types.Any, error) {
	return nil, fmt.Errorf("%s", toString(args, " "))
}

func set(args *types.Pair, env *envir.Env) (types.Any, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}

	val, err := Eval(args.Next.This, env)
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
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}
