package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

var _ Ellipsis = (*EllipsisSymbol)(nil)
var _ Ellipsis = (*EllipsisPair)(nil)

type Ellipsis interface {
	Transform(*MappingIterator) (*types.Pair, error)
}

type EllipsisSymbol types.Symbol

func (t EllipsisSymbol) String() string {
	return fmt.Sprintf("%s ...", types.Symbol(t))
}

func (t EllipsisSymbol) Transform(m *MappingIterator) (*types.Pair, error) {
	if !isValidEllipsis(t, m) {
		return nil, &ErrInvalidTemplate{t}
	}

	key := types.Symbol(t)
	val, err := m.GetEllipsis(key)
	if err != nil {
		return &types.Pair{}, err
	}

	switch val := val.(type) {
	case pattern.EllipsisVar:
		return types.NewPair(val...), nil
	default:
		return types.NewPair(val), nil
	}
}

type EllipsisPair types.Pair

func (t EllipsisPair) String() string {
	return fmt.Sprintf("%s ...", types.Pair(t))
}

func (t EllipsisPair) Transform(m *MappingIterator) (*types.Pair, error) {
	if !isValidEllipsis(t, m) {
		return nil, &ErrInvalidTemplate{t}
	}

	var (
		pair = types.Pair(t)
		ap   = types.NewAppendablePair()
		m2   = m.NextLevel()
	)
	for {
		val, err := transformPair(&pair, m2)

		switch {
		case err == ErrEllipsisOutOfBounds || err == ErrEmptyEllipsis:
			return ap.ToPair(), nil
		case err != nil:
			return nil, err
		}

		ap.Append(val)
		m2.Next()
	}
}

func isValidEllipsis(obj types.Sexpr, m *MappingIterator) bool {
	switch obj := obj.(type) {
	case EllipsisSymbol:
		return m.HasEllipsisVar(types.Symbol(obj))
	case EllipsisPair:
		pair := types.Pair(obj)
		return isValidEllipsis(&pair, m)
	case types.Symbol:
		return m.HasEllipsisVar(obj)
	case *types.Pair:
		head := obj
		for head != nil {
			if isValidEllipsis(head.This, m) {
				return true
			}
			head = head.Next
		}
		return false
	default:
		return false
	}
}
