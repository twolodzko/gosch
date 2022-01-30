package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/types"
)

// TODO: int->float, float->int

func isNumber(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.Integer, types.Float:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isInteger(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.Integer:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isFloat(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.Float:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func sum(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr = types.Integer(0)
		err    error
	)
	if args == nil {
		return result, nil
	}
	head := args
	for head != nil {
		switch x := result.(type) {
		case types.Arithmetic:
			result, err = x.Add(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not a number", x)
		}
		head = head.Next
	}
	return result, nil
}

func dif(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	if args == nil {
		return types.Integer(0), nil
	}
	if !args.HasNext() {
		switch x := args.This.(type) {
		case types.Integer:
			return -x, nil
		case types.Float:
			return -x, nil
		default:
			return nil, fmt.Errorf("%v is not a number", args.This)
		}
	}
	result = args.This
	head := args.Next
	for head != nil {
		switch x := result.(type) {
		case types.Arithmetic:
			result, err = x.Sub(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not a number", x)
		}
		head = head.Next
	}
	return result, nil
}

func mul(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr = types.Integer(1)
		err    error
	)
	if args == nil {
		return result, nil
	}
	head := args
	for head != nil {
		switch x := result.(type) {
		case types.Arithmetic:
			result, err = x.Mul(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not a number", x)
		}
		head = head.Next
	}
	return result, nil
}

func div(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr = types.Float(1)
		err    error
	)
	if args == nil {
		return types.Integer(1), nil
	}
	if !args.HasNext() {
		switch x := args.This.(type) {
		case types.Arithmetic:
			return types.Float(1).Div(x)
		default:
			return nil, fmt.Errorf("%v is not a number", args.This)
		}
	}
	result = args.This
	head := args.Next
	for head != nil {
		switch x := result.(type) {
		case types.Arithmetic:
			result, err = x.Div(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not a number", x)
		}
		head = head.Next
	}
	return result, nil
}

func mod(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch x := args.This.(type) {
	case types.Arithmetic:
		return x.Mod(args.Next.This)
	default:
		return nil, fmt.Errorf("%v is not a number", x)
	}
}
