package pattern

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type Subpattern interface {
	Match(types.Sexpr) (mapping.Mapping, bool)
	MatchEllipsis(*types.Pair, mapping.Mapping) (mapping.Mapping, bool)
	ToEllipsis()
	IsEllipsis() bool
}

type Pattern []Subpattern

func (p Pattern) Match(pair *types.Pair) (mapping.Mapping, bool) {
	return matchAll(p, pair)
}

func (p Pattern) String() string {
	var s []string
	for _, v := range p {
		s = append(s, fmt.Sprintf("%v", v))
	}
	return strings.Join(s, " ")
}

func matchAll(pattern []Subpattern, pair *types.Pair) (mapping.Mapping, bool) {
	m := mapping.Mapping{}
	head := pair
	for _, p := range pattern {
		if p.IsEllipsis() {
			return p.MatchEllipsis(head, m)
		}
		if head == nil {
			return mapping.Mapping{}, false
		}

		m2, ok := p.Match(head.This)
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
