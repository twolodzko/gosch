package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

type Ellipsis interface {
	Transform(*MappingIterator) *types.Pair
}

type EllipsisSymbol types.Symbol

func (t EllipsisSymbol) String() string {
	return fmt.Sprintf("%s ...", types.Symbol(t))
}

func (t EllipsisSymbol) Transform(m *MappingIterator) *types.Pair {
	key := types.Symbol(t)
	if val, ok := m.Get(key); ok {
		switch val := val.(type) {
		case pattern.EllipsisVar:
			return types.NewPair(val...)
		default:
			return types.NewPair(val)
		}
	}
	return types.NewPair(t)
}

type EllipsisPair types.Pair

func (t EllipsisPair) String() string {
	return fmt.Sprintf("%s ...", types.Pair(t))
}

func (t EllipsisPair) Transform(m *MappingIterator) *types.Pair {
	pair := types.Pair(t)
	ap := types.NewAppendablePair()
	m2 := m.Deeper()

	for {
		val, ok := elemPair(&pair, m2)
		if !ok {
			return ap.ToPair()
		}
		ap.Append(val)
		m2.Next()
	}
}

func elemPair(pair *types.Pair, m *MappingIterator) (*types.Pair, bool) {
	ap := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			val := obj.Transform(m)
			ap.Extend(val)
		case types.Symbol:
			if !m.Has(obj) {
				ap.Append(obj)
			} else {
				val, ok := m.GetEllipsis(obj)
				if !ok {
					return nil, ok
				}
				ap.Append(val)
			}
		case *types.Pair:
			val, ok := elemPair(obj, m)
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
