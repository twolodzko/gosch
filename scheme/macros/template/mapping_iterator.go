package template

import (
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

type MappingIterator struct {
	Mapping mapping.Mapping
	Index   []int
}

func NewMappingIterator(m mapping.Mapping) *MappingIterator {
	return &MappingIterator{m, []int{}}
}

func (m MappingIterator) Get(key types.Symbol) (types.Sexpr, error) {
	val := m.Mapping[key]
	if !m.IsNestedQuery() {
		return val, nil
	}
	switch val := val.(type) {
	case pattern.EllipsisVar:
		for _, i := range m.Index {
			if i >= len(val) {
				return nil, ErrEllipsisOutOfBounds
			}
			switch obj := val[i].(type) {
			case pattern.EllipsisVar:
				val = obj
			default:
				return obj, nil
			}
		}
		return val, nil
	default:
		return val, nil
	}
}

func (m MappingIterator) GetEllipsis(key types.Symbol) (types.Sexpr, error) {
	val := m.Mapping[key]
	if !m.IsNestedQuery() {
		return val, nil
	}
	switch val := val.(type) {
	case pattern.EllipsisVar:
		for _, i := range m.Index {
			if !val.IsNested() {
				break
			}
			if i >= len(val) {
				return nil, ErrEllipsisOutOfBounds
			}
			val = val[i].(pattern.EllipsisVar)
		}

		if len(val) == 0 {
			return nil, ErrEmptyEllipsis
		}
		return val, nil
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
	if m.IsNestedQuery() {
		m.Index[len(m.Index)-1]++
	}
}

func (m MappingIterator) IsNestedQuery() bool {
	return len(m.Index) > 0
}
