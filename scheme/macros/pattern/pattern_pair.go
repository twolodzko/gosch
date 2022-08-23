package pattern

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type Pair struct {
	Patterns []Pattern
	Repeated bool
}

func (p Pair) Match(obj types.Sexpr) (mapping.Mapping, bool) {
	pair, ok := obj.(*types.Pair)
	if !ok {
		return mapping.Mapping{}, false
	}
	if len(p.Patterns) == 0 {
		return mapping.Mapping{}, pair == nil || bool(pair.IsNull())
	}

	m := mapping.Mapping{}
	head := pair
	for _, pattern := range p.Patterns {
		if head == nil {
			return mapping.Mapping{}, false
		}

		if pattern.IsEllipsis() {
			return matchEllipsis(pattern, head, m)
		}

		m2, ok := pattern.Match(head.This)
		if !ok {
			return mapping.Mapping{}, false
		}
		ok = m.Merge(m2)
		if !ok {
			return mapping.Mapping{}, false
		}

		head = head.Next
	}

	// input longer than the pattern
	if head != nil {
		return mapping.Mapping{}, false
	}
	return m, true
}

func matchEllipsis(pattern Pattern, pair *types.Pair, m mapping.Mapping) (mapping.Mapping, bool) {
	switch pattern := pattern.(type) {
	case *Pair:
		subpattern, ok := pattern.matchPairEllipsis(pair)
		if !ok {
			return mapping.Mapping{}, false
		}
		ok = m.Merge(subpattern)
		return m, ok
	case *Identifier:
		if val, ok := ellipsisVarFromPair(pair); ok {
			m[pattern.Name] = val
		}
		return m, true
	default:
		return mapping.Mapping{}, false
	}
}

func (p *Pair) matchPairEllipsis(pair *types.Pair) (mapping.Mapping, bool) {
	mappings := mapping.Mapping{}
	head := pair
	for head != nil {
		m, ok := p.Match(head.This)
		if !ok {
			return mapping.Mapping{}, false
		}
		mappings = extendMapping(mappings, m)
		head = head.Next
	}
	return mappings, true
}

func extendMapping(x mapping.Mapping, y mapping.Mapping) mapping.Mapping {
	for key, yval := range y {
		var e EllipsisVar
		if xval, ok := x[key]; ok {
			switch xval := xval.(type) {
			case EllipsisVar:
				e = xval
			default:
				e = EllipsisVar{xval}
			}
			e = append(e, yval)
		} else {
			e = EllipsisVar{yval}
		}
		x[key] = e
	}
	return x
}

func (p Pair) String() string {
	var str []string
	for _, v := range p.Patterns {
		str = append(str, fmt.Sprintf("%s", v))
	}
	if p.Repeated {
		return fmt.Sprintf("(%s) ...", strings.Join(str, " "))
	}
	return fmt.Sprintf("(%s)", strings.Join(str, " "))
}

func (p *Pair) ToEllipsis() {
	p.Repeated = true
}

func (p Pair) IsEllipsis() bool {
	return p.Repeated
}
