package special_forms

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.Procedure = Quote
var _ eval.Procedure = QuasiQuote
var _ eval.Procedure = Unquote

func Quote(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, eval.ErrSyntax
	}
	if p.Next != nil {
		return nil, eval.ErrArity
	}
	return p.This, nil
}

// `unquote` procedure
func Unquote(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, eval.ErrSyntax
	}
	if p.Next != nil {
		return nil, eval.ErrArity
	}
	return eval.Eval(p.This, env)
}

// `quasiQuote` procedure
func QuasiQuote(args any, env *envir.Env) (any, error) {
	p, ok := args.(types.Pair)
	if !ok {
		return nil, eval.ErrSyntax
	}
	if p.Next != nil {
		return nil, eval.ErrArity
	}
	return unquoteRecursively(p.This, 1, env)
}

func unquoteRecursively(val any, numQuotes int, env *envir.Env) (any, error) {
	p, ok := val.(types.Pair)
	if !ok {
		return val, nil
	}
	if sym, ok := p.This.(types.Symbol); ok {
		switch sym {
		case "quasiquote":
			numQuotes++
		case "unquote":
			numQuotes--
			if numQuotes == 0 {
				return Unquote(p.Next, env)
			}
		}
	}
	head, err := unquoteRecursively(p.This, numQuotes, env)
	if err != nil {
		return nil, err
	}
	tail, err := unquoteRecursively(p.Next, numQuotes, env)
	if err != nil {
		return nil, err
	}
	return types.Cons(head, tail), err
}
