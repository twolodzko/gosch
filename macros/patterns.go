package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/types"
)

type Mapping map[types.Symbol]types.Sexpr

func mergeMappings(x, y Mapping) (Mapping, bool) {
	for key, val := range y {
		if _, ok := x[key]; ok {
			return Mapping{}, false
		}
		x[key] = val
	}
	return x, true
}

type Pattern interface {
	Match(types.Sexpr) (Mapping, bool)
}

type PairPattern struct {
	Values []Pattern
}

func (p PairPattern) Match(obj types.Sexpr) (Mapping, bool) {
	pair, ok := obj.(*types.Pair)
	if !ok {
		return Mapping{}, false
	}
	if len(p.Values) == 0 {
		return Mapping{}, bool(pair.IsNull())
	}

	mapping := Mapping{}
	head := pair
	for _, pattern := range p.Values {
		fmt.Println(pattern, head)
		switch pattern.(type) {
		case EllipsisPattern:
			if head == nil {
				return mapping, true
			}
			mapping[Ellipsis] = head
			return mapping, true
		default:
			if head == nil {
				return Mapping{}, false
			}
			matched, ok := pattern.Match(head.This)
			if !ok {
				return Mapping{}, false
			}
			mapping, ok = mergeMappings(mapping, matched)
			if !ok {
				return Mapping{}, false
			}
		}
		head = head.Next
	}

	return mapping, true
}

func (p PairPattern) String() string {
	var str []string
	for _, v := range p.Values {
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
	return Mapping{}, obj == types.Symbol("...")
}

func (p EllipsisPattern) String() string {
	return Ellipsis
}

type SelfPattern struct {
	Name types.Symbol
}

func (p SelfPattern) Match(obj types.Sexpr) (Mapping, bool) {
	return Mapping{}, obj == types.Symbol("_")
}

func (p SelfPattern) String() string {
	return Self
}
