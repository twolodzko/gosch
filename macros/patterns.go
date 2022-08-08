package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/types"
)

type Pattern interface {
	Match(types.Sexpr) (Mapping, bool)
}

type PairPattern struct {
	Patterns []Pattern
}

func (p PairPattern) Match(obj types.Sexpr) (Mapping, bool) {
	pair, ok := obj.(*types.Pair)
	if !ok {
		return Mapping{}, false
	}

	patterns := p.Patterns

	if len(patterns) == 0 {
		return Mapping{}, bool(pair == nil || pair.IsNull())
	}

	mapping := Mapping{}
	head := pair
	for _, pattern := range patterns {
		switch pattern.(type) {
		case EllipsisPattern:
			if head == nil || head.IsNull() {
				return mapping, true
			}
			mapping[Ellipsis] = ToEllypsisVars(head)
			return mapping, true
		default:
			if head == nil {
				return Mapping{}, false
			}
			subpattern, ok := pattern.Match(head.This)
			if !ok {
				return Mapping{}, false
			}
			mapping, ok = mergeMappings(mapping, subpattern)
			if !ok {
				return Mapping{}, false
			}
		}
		head = head.Next
	}

	if head != nil {
		return Mapping{}, false
	}
	return mapping, true
}

func (p PairPattern) String() string {
	var str []string
	for _, v := range p.Patterns {
		str = append(str, fmt.Sprintf("%s", v))
	}
	return fmt.Sprintf("(%s)", strings.Join(str, " "))
}

type IdentifierPattern struct {
	Name types.Symbol
}

func (p IdentifierPattern) Match(obj types.Sexpr) (Mapping, bool) {
	if obj == nil {
		return Mapping{}, false
	}
	return Mapping{p.Name: obj}, true
}

func (p IdentifierPattern) String() string {
	return p.Name
}

type LiteralPattern struct {
	Value types.Sexpr
}

func (p LiteralPattern) Match(obj types.Sexpr) (Mapping, bool) {
	return Mapping{}, obj == p.Value
}

func (p LiteralPattern) String() string {
	return fmt.Sprintf("%s", p.Value)
}

type EllipsisPattern struct{}

func (p EllipsisPattern) Match(obj types.Sexpr) (Mapping, bool) {
	return Mapping{}, obj == Ellipsis
}

func (p EllipsisPattern) String() string {
	return Ellipsis
}
