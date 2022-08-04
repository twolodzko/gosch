package scheme

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `quote` procedure
func Quote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	return args.This, nil
}

// `quasiquote` procedure
func Quasiquote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	pair, ok := args.This.(*types.Pair)
	if !ok || pair == nil || !bool(pair.HasNext()) {
		return args.This, nil
	}
	return evalUnquotes(pair, env)
}

// `unquote` procedure
func Unquote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	return eval.Eval(args.This, env)
}

func evalUnquotes(pair *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if pair == nil {
		return pair, nil
	}
	if sym, ok := pair.This.(types.Symbol); ok && sym == "unquote" {
		return Unquote(pair.Next, env)
	}

	ap := types.NewAppendablePair()
	head := pair
	for head != nil {
		if p, ok := head.This.(*types.Pair); ok {
			val, err := evalUnquotes(p, env)
			if err != nil {
				return nil, err
			}
			ap.Append(val)
		} else {
			ap.Append(head.This)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}
