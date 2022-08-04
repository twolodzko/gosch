package scheme

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `quote` procedure
func Quote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	return args.This, nil
}

// `quasiquote` procedure
func Quasiquote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	pair, ok := args.This.(*types.Pair)
	if !ok || pair == nil || !bool(pair.HasNext()) {
		return args.This, nil
	}
	return traverseToUnquote(pair, 1, env)
}

// `unquote` procedure
func Unquote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	return eval.Eval(args.This, env)
}

func traverseToUnquote(pair *types.Pair, numQuotes int, env *envir.Env) (types.Sexpr, error) {
	if pair == nil {
		return pair, nil
	}
	if sym, ok := pair.This.(types.Symbol); ok {
		switch sym {
		case "quasiquote":
			numQuotes++
		case "unquote":
			numQuotes--
			if numQuotes == 0 {
				return Unquote(pair.Next, env)
			}
		}
	}

	// iterate through all the elements of the list
	ap := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case *types.Pair:
			// check nested lists recursively
			val, err := traverseToUnquote(obj, numQuotes, env)
			if err != nil {
				return nil, err
			}
			ap.Append(val)
		default:
			ap.Append(head.This)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}
