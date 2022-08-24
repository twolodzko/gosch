package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

type Ellipsis interface {
	Transform(mapping.Mapping) *types.Pair
}

type EllipsisSymbol types.Symbol

func (t EllipsisSymbol) String() string {
	return fmt.Sprintf("%s ...", types.Symbol(t))
}

func (t EllipsisSymbol) Transform(m mapping.Mapping) *types.Pair {
	key := types.Symbol(t)
	if val, ok := m[key]; ok {
		switch val := val.(type) {
		case pattern.EllipsisVar:
			return types.PairFromArray(val)
		default:
			return types.NewPair(val, nil)
		}
	}
	return types.NewPair(t, nil)
}

type EllipsisPair types.Pair

func (t EllipsisPair) String() string {
	return fmt.Sprintf("%s ...", types.Pair(t))
}

func (t EllipsisPair) Transform(m mapping.Mapping) *types.Pair {
	pair := types.Pair(t)
	ap := types.NewAppendablePair()
	i := 0
	for {
		val, ok := elemPair(&pair, m, i)
		if !ok {
			return ap.ToPair()
		}
		ap.Append(val)
		i++
	}
}

func elemPair(pair *types.Pair, m mapping.Mapping, i int) (*types.Pair, bool) {
	ap := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			val := obj.Transform(m)
			ap.Extend(val)
		case types.Symbol:
			val, ok := elemSymbol(obj, m, i)
			if !ok {
				return nil, ok
			}
			ap.Append(val)
		case *types.Pair:
			val, ok := elemPair(obj, m, i)
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

func elemSymbol(sym types.Symbol, m mapping.Mapping, i int) (types.Sexpr, bool) {
	val := expandSymbol(sym, m)
	if ellipsis, ok := val.(pattern.EllipsisVar); ok {
		if i >= len(ellipsis) {
			return nil, false
		}
		val = ellipsis[i]
	}
	return val, true
}
