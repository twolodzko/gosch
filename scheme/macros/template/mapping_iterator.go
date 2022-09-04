package template

import (
	"errors"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

var ErrNoKey = errors.New("unknown mapping")

type MappingIterator struct {
	Mapping mapping.Mapping
	Index   []int
}

func NewMappingIterator(m mapping.Mapping) *MappingIterator {
	return &MappingIterator{m, []int{}}
}

func (m MappingIterator) Get(key types.Symbol) (types.Sexpr, error) {
	val, ok := m.Mapping[key]
	if !m.IsNested() {
		if !ok {
			return nil, ErrNoKey
		}
		return val, nil
	}
	switch val := val.(type) {
	case pattern.EllipsisVar:
		return extractEllipsisVar(val, m.Index, false)
	default:
		return val, nil
	}
}

func (m MappingIterator) GetEllipsis(key types.Symbol) (types.Sexpr, error) {
	val, ok := m.Mapping[key]
	if !m.IsNested() {
		if !ok {
			return nil, ErrNoKey
		}
		return val, nil
	}
	switch val := val.(type) {
	case pattern.EllipsisVar:
		return extractEllipsisVar(val, m.Index, true)
	default:
		return val, nil
	}
}

func (m *MappingIterator) Set(key types.Symbol, val types.Sexpr) {
	m.Mapping[key] = val
}

func (m MappingIterator) Has(key types.Symbol) bool {
	_, ok := m.Mapping[key]
	return ok
}

func (m MappingIterator) Copy() *MappingIterator {
	return NewMappingIterator(m.Mapping.Copy())
}

func (m MappingIterator) NextLevel() *MappingIterator {
	mapping := m.Mapping.Copy()
	index := append(m.Index, 0)
	return &MappingIterator{mapping, index}
}

func (m *MappingIterator) Next() {
	if m.IsNested() {
		m.Index[len(m.Index)-1]++
	}
}

func (m MappingIterator) IsNested() bool {
	return len(m.Index) > 0
}

func extractEllipsisVar(val pattern.EllipsisVar, index []int, ellipsis bool) (types.Sexpr, error) {
	for _, i := range index {
		if i >= len(val) {
			if len(val) == 0 {
				return nil, ErrEllipsisOutOfBounds
			}
			switch val[0].(type) {
			case pattern.EllipsisVar:
				// has no following items
				return nil, ErrEllipsisOutOfBounds
			default:
				// is the upmost level
				if ellipsis {
					return val, nil
				} else {
					return nil, ErrEllipsisOutOfBounds
				}
			}
		}

		switch obj := val[i].(type) {
		case pattern.EllipsisVar:
			val = obj
		default:
			// upmost level
			if ellipsis {
				return val, nil
			} else {
				return obj, nil
			}
		}
	}
	return val, nil
}
