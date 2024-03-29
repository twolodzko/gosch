package primitives

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `number?` procedure
func IsNumber(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Integer, types.Float:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `integer?` procedure
func IsInteger(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Integer:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `float?` procedure
func IsFloat(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Float:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `->int` procedure
func ToInt(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
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
func ToFloat(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
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
func Sum(args *types.Pair) (types.Sexpr, error) {
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
func Dif(args *types.Pair) (types.Sexpr, error) {
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
func Mul(args *types.Pair) (types.Sexpr, error) {
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
func Div(args *types.Pair) (types.Sexpr, error) {
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
func Mod(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	switch prev := args.This.(type) {
	case types.Arithmetic:
		return prev.Mod(args.Next.This)
	default:
		return nil, &types.ErrNaN{Val: prev}
	}
}

// `//` procedure
func IntDiv(args *types.Pair) (types.Sexpr, error) {
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
