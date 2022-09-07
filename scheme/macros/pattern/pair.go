package pattern

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

var _ Subpattern = (*Literal)(nil)

type Pair struct {
	Patterns []Subpattern
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
	return matchAll(p.Patterns, pair)
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

func (p *Pair) MatchEllipsis(pair *types.Pair, m mapping.Mapping) (mapping.Mapping, bool) {
	if pair == nil || pair.IsNull() {
		for k, v := range p.EmptyMatch() {
			m[k] = mapping.Ellipsis{v}
		}
		return m, true
	}

	head := pair
	for head != nil {
		m2, ok := p.Match(head.This)
		if !ok {
			return mapping.Mapping{}, false
		}
		m.Extend(m2)
		head = head.Next
	}
	return m, true
}

func (p *Pair) EmptyMatch() mapping.Mapping {
	m := mapping.Mapping{}
	for _, s := range p.Patterns {
		switch s := s.(type) {
		case *Identifier:
			m[s.Name] = mapping.Ellipsis{}
		case *Pair:
			for k, v := range s.EmptyMatch() {
				m[k] = mapping.Ellipsis{v}
			}
		}
	}
	return m
}
