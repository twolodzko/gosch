package template

import (
	"github.com/twolodzko/gosch/types"
)

type Template interface {
	Transform(*MappingIterator) (types.Sexpr, error)
}

func Transform(sexpr types.Sexpr, m *MappingIterator) (types.Sexpr, error) {
	switch obj := sexpr.(type) {
	case types.Symbol:
		return expandSymbol(obj, m), nil
	case *types.Pair:
		return transformPair(obj, m)
	case Template:
		return obj.Transform(m)
	default:
		return obj, nil
	}
}

func expandSymbol(s types.Symbol, m *MappingIterator) types.Sexpr {
	if val, ok := m.Get(s); ok {
		return val
	}
	return s
}

func transformPair(p *types.Pair, m *MappingIterator) (*types.Pair, error) {
	ap := types.NewAppendablePair()
	head := p
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			val, _ := obj.Transform(m)
			ap.Extend(val)
		case Template:
			val, err := obj.Transform(m)
			if err != nil {
				return nil, err
			}
			ap.Append(val)
		case types.Symbol:
			val := expandSymbol(obj, m)
			ap.Append(val)
		case *types.Pair:
			val, err := transformPair(obj, m)
			if err != nil {
				return nil, err
			}
			ap.Append(val)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}
