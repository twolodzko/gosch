package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

var _ Ellipsis = (*EllipsisSymbol)(nil)
var _ Ellipsis = (*EllipsisPair)(nil)

type Ellipsis interface {
	Transform(*MappingIterator) (*types.Pair, bool)
}

type EllipsisSymbol types.Symbol

func (t EllipsisSymbol) String() string {
	return fmt.Sprintf("%s ...", types.Symbol(t))
}

func (t EllipsisSymbol) Transform(m *MappingIterator) (*types.Pair, bool) {
	key := types.Symbol(t)
	if val, ok := m.GetEllipsis(key); ok {
		switch val := val.(type) {
		case pattern.EllipsisVar:
			return types.NewPair(val...), true
		default:
			return types.NewPair(val), true
		}
	}
	return &types.Pair{}, false
}

type EllipsisPair types.Pair

func (t EllipsisPair) String() string {
	return fmt.Sprintf("%s ...", types.Pair(t))
}

func (t EllipsisPair) Transform(m *MappingIterator) (*types.Pair, bool) {
	pair := types.Pair(t)
	ap := types.NewAppendablePair()
	m2 := m.NextLevel()

	for {
		val, ok := elemPair(&pair, m2)
		if !ok {
			return ap.ToPair(), ok
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
			val, ok := obj.Transform(m)
			if !ok {
				return nil, ok
			}
			ap.Extend(val)
		case types.Symbol:
			if !m.Has(obj) {
				ap.Append(obj)
			} else {
				val, ok := m.Get(obj)
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
