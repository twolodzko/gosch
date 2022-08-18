package template

import (
	"fmt"

	"github.com/twolodzko/gosch/macros/mapping"
	"github.com/twolodzko/gosch/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

type Ellipsis struct {
	Sexpr types.Sexpr
}

func (e Ellipsis) String() string {
	return fmt.Sprintf("%s ...", e.Sexpr)
}

func (t Ellipsis) Transform(m mapping.Mapping) *types.Pair {
	switch sexpr := t.Sexpr.(type) {
	case types.Symbol:
		return transformEllipsisSymbol(sexpr, m)
	case *types.Pair:
		return transformEllipsisPair(sexpr, m)
	default:
		return types.NewPair(sexpr, nil)
	}
}

func transformEllipsisSymbol(sexpr types.Symbol, m mapping.Mapping) *types.Pair {
	if val, ok := m[sexpr]; ok {
		switch val := val.(type) {
		case pattern.EllipsisVar:
			return types.PairFromArray(val)
		default:
			return types.NewPair(val, nil)
		}
	}
	return types.NewPair(sexpr, nil)
}

func transformEllipsisPair(pair *types.Pair, m mapping.Mapping) *types.Pair {
	ap := types.NewAppendablePair()
	i := 0
	for {
		val, ok := transformEllipsisPairStep(pair, m, i)
		if !ok {
			return ap.ToPair()
		}
		ap.Append(val)
		i++
	}
}

func transformEllipsisPairStep(pair *types.Pair, m mapping.Mapping, i int) (*types.Pair, bool) {
	ap := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			ap.Extend(obj.Transform(m))
		case types.Symbol:
			val, ok := transformSymbolStep(obj, m, i)
			if !ok {
				return nil, ok
			}
			ap.Append(val)
		case *types.Pair:
			val, ok := transformEllipsisPairStep(obj, m, i)
			if !ok {
				return nil, ok
			}
			ap.Append(val)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair(), true
}

func transformSymbolStep(sym types.Symbol, m mapping.Mapping, i int) (types.Sexpr, bool) {
	val := transformSymbol(sym, m)
	if ellipsis, ok := val.(pattern.EllipsisVar); ok {
		if i >= len(ellipsis) {
			return nil, false
		}
		val = ellipsis[i]
	}
	return val, true
}
