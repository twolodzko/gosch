package primitives

import (
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `null?` procedure
func IsNull(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.IsNull(), nil
	default:
		return types.FALSE, nil
	}
}

// `pair?` procedure
func IsPair(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return !val.IsNull(), nil
	default:
		return types.FALSE, nil
	}
}

// `bool?` procedure
func IsBool(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Bool:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `symbol?` procedure
func IsSymbol(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Symbol:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `nil?` procedure
func IsNil(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	return types.Bool(args.This == nil), nil
}

// `procedure?` procedure
func IsProcedure(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	return types.Bool(isCallable(args.This)), nil
}

func isCallable(obj types.Sexpr) bool {
	switch obj.(type) {
	case eval.Procedure, eval.Primitive, eval.TailCallOptimized, eval.Callable:
		return true
	default:
		return false
	}
}
