package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/types"
)

type Pattern interface {
	Match(types.Sexpr) (Mappings, bool)
}

type PairPattern struct {
	Patterns []Pattern
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
		switch pattern.(type) {
		case EllipsisPattern:
			if head == nil || head.IsNull() {
				return mapping, true
			}
			mapping[Ellipsis] = head
			return mapping, true
		default:
			if head == nil {
				return Mappings{}, false
			}
			subpattern, ok := pattern.Match(head.This)
			if !ok {
				return Mappings{}, false
			}
			mapping, ok = mergeMappings(mapping, subpattern)
			if !ok {
				return Mappings{}, false
			}
		}
		head = head.Next
	}

	// input longer than the pattern
	if head != nil {
		return Mappings{}, false
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

func (p IdentifierPattern) Match(obj types.Sexpr) (Mappings, bool) {
	if obj == nil {
		return Mappings{}, false
	}
	return Mappings{p.Name: obj}, true
}

func (p IdentifierPattern) String() string {
	return p.Name
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

type EllipsisPattern struct{}

func (p EllipsisPattern) Match(obj types.Sexpr) (Mappings, bool) {
	return Mappings{}, obj == Ellipsis
}

func (p EllipsisPattern) String() string {
	return Ellipsis
}
