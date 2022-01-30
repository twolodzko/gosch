package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

func isNumber(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
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
		return nil, ErrBadArgNumber
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
		return nil, ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Float:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func toInt(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, ErrBadArgNumber
	}
	switch num := args.This.(type) {
	case types.Integer:
		return num, nil
	case types.Float:
		return types.Integer(num), nil
	default:
		return nil, &types.ErrNaN{Val: num}
	}
}

func toFloat(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, ErrBadArgNumber
	}
	switch num := args.This.(type) {
	case types.Integer:
		return types.Float(num), nil
	case types.Float:
		return num, nil
	default:
		return nil, &types.ErrNaN{Val: num}
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
		switch prev := result.(type) {
		case types.Arithmetic:
			result, err = prev.Add(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, &types.ErrNaN{Val: prev}
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
		switch prev := args.This.(type) {
		case types.Integer:
			return -prev, nil
		case types.Float:
			return -prev, nil
		default:
			return nil, &types.ErrNaN{Val: args.This}
		}
	}
	result = args.This
	head := args.Next
	for head != nil {
		switch prev := result.(type) {
		case types.Arithmetic:
			result, err = prev.Sub(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, &types.ErrNaN{Val: prev}
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
		switch prev := result.(type) {
		case types.Arithmetic:
			result, err = prev.Mul(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, &types.ErrNaN{Val: prev}
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
		switch prev := args.This.(type) {
		case types.Arithmetic:
			return types.Float(1).Div(prev)
		default:
			return nil, &types.ErrNaN{Val: args.This}
		}
	}
	result = args.This
	head := args.Next
	for head != nil {
		switch prev := result.(type) {
		case types.Arithmetic:
			result, err = prev.Div(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, &types.ErrNaN{Val: prev}
		}
		head = head.Next
	}
	return result, nil
}

func mod(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, ErrBadArgNumber
	}
	switch prev := args.This.(type) {
	case types.Arithmetic:
		return prev.Mod(args.Next.This)
	default:
		return nil, &types.ErrNaN{Val: prev}
	}
}

func intDiv(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr = types.Integer(1)
		err    error
	)
	if args == nil || !args.HasNext() {
		return types.Integer(0), nil
	}
	result = args.This
	head := args.Next
	for head != nil {
		switch prev := result.(type) {
		case types.Integer:
			result, err = prev.IntDiv(head.This)
			if err != nil {
				return nil, err
			}
		default:
			return nil, fmt.Errorf("%v is not an integer", prev)
		}
		head = head.Next
	}
	return result, nil
}
