package template

import (
	"github.com/twolodzko/gosch/scheme/macros/mapping"
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
	switch val := val.(type) {
	case mapping.Ellipsis:
		for _, i := range m.Index {
			if i >= len(val) {
				return nil, ErrEllipsisOutOfBounds
			}
			switch obj := val[i].(type) {
			case mapping.Ellipsis:
				val = obj
			default:
				return obj, nil
			}
		}
		return val, ErrInvalidEllipsis
	default:
		return val, nil
	}
}

func (m MappingIterator) GetEllipsis(key types.Symbol) (mapping.Ellipsis, error) {
	val := m.Mapping[key]
	switch val := val.(type) {
	case mapping.Ellipsis:
		for _, i := range m.Index {
			if !val.ContainsEllipses() {
				break
			}
			if i >= len(val) {
				return nil, ErrEllipsisOutOfBounds
			}
			val = val[i].(mapping.Ellipsis)
		}

		if m.IsNestedQuery() && len(val) == 0 {
			return nil, ErrEmptyEllipsis
		}
		return val, nil
	default:
		return maybeEllipsis(val)
	}
}

func maybeEllipsis(obj types.Sexpr) (mapping.Ellipsis, error) {
	switch val := obj.(type) {
	case mapping.Ellipsis:
		return val, nil
	default:
		return nil, ErrInvalidEllipsis
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

func (m MappingIterator) ContainsEllipsis(sym types.Symbol) bool {
	if val, exists := m.Mapping[sym]; exists {
		_, ok := val.(mapping.Ellipsis)
		return ok
	}
	return false
}
