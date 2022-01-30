package eval

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

// `number?` procedure
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

// `integer?` procedure
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

// `float?` procedure
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

// `->int` procedure
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

// `->float` procedure
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

// `+` procedure
func sum(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	if args == nil {
		return types.Integer(0), nil
	}
	if !args.HasNext() {
		switch val := args.This.(type) {
		case types.Arithmetic:
			return val, nil
		default:
			return nil, &types.ErrNaN{Val: val}
		}
	}
	result = args.This
	head := args.Next
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

// `-` procedure
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

// `*` procedure
func mul(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	if args == nil {
		return types.Integer(1), nil
	}
	if !args.HasNext() {
		switch val := args.This.(type) {
		case types.Arithmetic:
			return val, nil
		default:
			return nil, &types.ErrNaN{Val: val}
		}
	}
	result = args.This
	head := args.Next
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

// `/` procedure
func div(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	if args == nil {
		return types.Integer(1), nil
	}
	if !args.HasNext() {
		return types.Integer(1).Div(args.This)
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

// `%` procedure
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

// `//` procedure
func intDiv(args *types.Pair) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	if args == nil {
		return types.Integer(1), nil
	}
	if !args.HasNext() {
		switch val := args.This.(type) {
		case types.Integer:
			return 1 / val, nil
		default:
			return nil, &types.ErrNaN{Val: val}
		}
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
