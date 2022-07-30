package procedures

import (
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func car(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.This, nil
	default:
		return nil, eval.NewErrNonList(args.This)
	}
}

func cdr(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
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
		return nil, eval.NewErrNonList(args.This)
	}
}

func cons(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	switch val := args.Next.This.(type) {
	case *types.Pair:
		return val.Cons(args.This), nil
	default:
		return list(args)
	}
}

func list(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return &types.Pair{}, nil
	}
	return args, nil
}
