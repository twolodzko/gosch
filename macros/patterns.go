package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/types"
)

type Pattern interface {
	Match(types.Sexpr) (Mappings, bool)
	ToEllipsis()
	IsEllipsis() bool
}

type PairPattern struct {
	Patterns []Pattern
	Repeated bool
}

func (p PairPattern) Match(obj types.Sexpr) (Mappings, bool) {
	pair, ok := obj.(*types.Pair)
	if !ok {
		return Mappings{}, false
	}
	if len(p.Patterns) == 0 {
		return Mappings{}, bool(pair == nil || pair.IsNull())
	}

	mapping := Mappings{}
	head := pair
	for _, pattern := range p.Patterns {

		if head == nil {
			return Mappings{}, false
		}

		if pattern.IsEllipsis() {
			switch pattern := pattern.(type) {
			case *PairPattern:
				subpattern, ok := pattern.matchPairEllipses(head)
				if !ok {
					return Mappings{}, false
				}
				ok = mapping.merge(subpattern)
				return mapping, ok
			case *IdentifierPattern:
				mapping[pattern.Name] = EllipsisVarFromPair(head)
				return mapping, true
			default:
				return Mappings{}, false
			}
		}

		subpattern, ok := pattern.Match(head.This)
		if !ok {
			return Mappings{}, false
		}
		ok = mapping.merge(subpattern)
		if !ok {
			return Mappings{}, false
		}

		head = head.Next
	}

	// input longer than the pattern
	if head != nil {
		return Mappings{}, false
	}
	return mapping, true
}

func (p *PairPattern) matchPairEllipses(pair *types.Pair) (Mappings, bool) {
	mappings := Mappings{}
	head := pair
	for head != nil {
		// FIXME
		// m, ok := p.Match(head.This)
		// if !ok {
		// 	return Mappings{}, false
		// }
		// mappings = mergePairEllipses(mappings, m)
		head = head.Next
	}
	return mappings, true
}

func (p PairPattern) String() string {
	var str []string
	for _, v := range p.Patterns {
		str = append(str, fmt.Sprintf("%s", v))
	}
	if p.Repeated {
		return fmt.Sprintf("(%s) ...", strings.Join(str, " "))
	}
	return fmt.Sprintf("(%s)", strings.Join(str, " "))
}

func (p *PairPattern) ToEllipsis() {
	p.Repeated = true
}

func (p PairPattern) IsEllipsis() bool {
	return p.Repeated
}

type IdentifierPattern struct {
	Name     types.Symbol
	Repeated bool
}

func (p IdentifierPattern) Match(obj types.Sexpr) (Mappings, bool) {
	if obj == nil {
		return Mappings{}, false
	}
	return Mappings{p.Name: obj}, true
}

func (p IdentifierPattern) String() string {
	if p.Repeated {
		return fmt.Sprintf("%s ...", p.Name)
	}
	return p.Name
}

func (p *IdentifierPattern) ToEllipsis() {
	p.Repeated = true
}

func (p IdentifierPattern) IsEllipsis() bool {
	return p.Repeated
}

type LiteralPattern struct {
	Value types.Sexpr
}

func (p LiteralPattern) Match(obj types.Sexpr) (Mappings, bool) {
	return Mappings{}, obj == p.Value
}

func (p LiteralPattern) String() string {
	return fmt.Sprintf("%s", p.Value)
}

func (p *LiteralPattern) ToEllipsis() {
	// cannot be an ellipsis
}

func (p LiteralPattern) IsEllipsis() bool {
	return false
}

type EllipsisVar []types.Sexpr

func (e EllipsisVar) ToPair() *types.Pair {
	return types.PairFromArray(e)
}

func EllipsisVarFromPair(pair *types.Pair) *EllipsisVar {
	var e EllipsisVar
	head := pair
	for head != nil {
		e = append(e, head.This)
		head = head.Next
	}
	return &e
}

func (e *EllipsisVar) Add(val types.Sexpr) {
	switch val := val.(type) {
	case EllipsisVar:
		(*e) = append(*e, val...)
	default:
		(*e) = append(*e, val)
	}
}
