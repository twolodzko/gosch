package macros

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

type SyntacticClosure struct {
	Sexpr types.Sexpr
	Env   *envir.Env
}

func (c SyntacticClosure) Get() (types.Sexpr, error) {
	return eval.Eval(c.Sexpr, c.Env)
}

func expand(template types.Sexpr, mapping Mapping, env *envir.Env) types.Sexpr {
	switch obj := template.(type) {
	case types.Symbol:
		if val, ok := mapping[obj]; ok {
			// val, _ := eval.Eval(val, env)
			return val
			// return SyntacticClosure{val, env}
		}
		return obj
	case *types.Pair:
		return expandPair(obj, mapping, env)
	default:
		return obj
	}
}

func expandPair(template *types.Pair, mapping Mapping, env *envir.Env) *types.Pair {
	ap := types.NewAppendablePair()
	head := template
	for head != nil {
		if sym, ok := head.This.(types.Symbol); ok && sym == Ellipsis {
			val := expandEllipsis(mapping, env)
			ap.Extend(val)
		} else {
			val := expand(head.This, mapping, env)
			ap.Append(val)
		}
		head = head.Next
	}
	return ap.ToPair()
}

func expandEllipsis(mapping Mapping, env *envir.Env) *types.Pair {
	ap := types.NewAppendablePair()
	for _, obj := range mapping[Ellipsis].(EllipsisVars) {
		val := expand(obj, mapping, env)
		ap.Append(val)
	}
	return ap.ToPair()
}
