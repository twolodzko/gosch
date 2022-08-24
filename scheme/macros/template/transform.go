package template

import (
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type Template interface {
	Transform(mapping.Mapping) (types.Sexpr, error)
}

func Transform(sexpr types.Sexpr, m mapping.Mapping) (types.Sexpr, error) {
	switch obj := sexpr.(type) {
	case types.Symbol:
		return expandSymbol(obj, m), nil
	case *types.Pair:
		return transformPair(obj, m), nil
	case Template:
		return obj.Transform(m)
	default:
		return obj, nil
	}
}

func expandSymbol(s types.Symbol, m mapping.Mapping) types.Sexpr {
	if val, ok := m[s]; ok {
		return val
	}
	return s
}

func transformPair(p *types.Pair, m mapping.Mapping) *types.Pair {
	ap := types.NewAppendablePair()
	head := p
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			val := obj.Transform(m)
			ap.Extend(val)
		case types.Symbol:
			val := expandSymbol(obj, m)
			ap.Append(val)
		case *types.Pair:
			val := transformPair(obj, m)
			ap.Append(val)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair()
}
