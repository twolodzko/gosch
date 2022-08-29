package pattern

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type Subpattern interface {
	Match(types.Sexpr) (mapping.Mapping, bool)
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
			return matchEllipsis(p, head, m)
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

func matchEllipsis(pattern Subpattern, pair *types.Pair, m mapping.Mapping) (mapping.Mapping, bool) {
	switch pattern := pattern.(type) {
	case *Pair:
		m2, ok := pattern.matchPairEllipsis(pair)
		if !ok {
			return mapping.Mapping{}, false
		}
		ok = m.Merge(m2)
		return m, ok
	case *Identifier:
		m[pattern.Name] = ellipsisVarFromPair(pair)
		return m, true
	default:
		return mapping.Mapping{}, false
	}
}

func (p *Pair) matchPairEllipsis(pair *types.Pair) (mapping.Mapping, bool) {
	m := mapping.Mapping{}

	if pair == nil || pair.IsNull() {
		return m, true
	}

	head := pair
	for head != nil {
		m2, ok := p.Match(head.This)
		if !ok {
			return mapping.Mapping{}, false
		}
		m = extendMapping(m, m2)
		head = head.Next
	}
	return m, true
}

// TODO: use to initialize mapping for empty pair pattern & validate
// func (p *Pair) allIdentifiers() []types.Symbol {
// 	var identifiers []types.Symbol
// 	for _, val := range p.Patterns {
// 		switch val := val.(type) {
// 		case *Identifier:
// 			identifiers = append(identifiers, val.Name)
// 		case *Pair:
// 			identifiers = append(identifiers, val.allIdentifiers()...)
// 		}
// 	}
// 	return identifiers
// }

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
