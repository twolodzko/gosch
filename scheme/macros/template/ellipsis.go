package template

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

var ErrEllipsisOutOfBounds = errors.New("ellipsis variable out of bounds")

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
	pair := types.Pair(t)
	ap := types.NewAppendablePair()
	m2 := m.NextLevel()

	for {
		val, err := transformPair(&pair, m2)

		if err == ErrEllipsisOutOfBounds {
			return ap.ToPair(), nil
		} else if err != nil {
			return nil, err
		}

		ap.Append(val)
		m2.Next()
	}
}
