package template

import (
	"fmt"

	"github.com/twolodzko/gosch/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type Template interface {
	Transform(mapping.Mapping) types.Sexpr
}

// type LambdaTemplate struct {
// 	Args *types.Pair
// 	Body *types.Pair
// }

// type LetTemplate struct {
// 	Binings *types.Pair
// 	Body    *types.Pair
// }

type Sexpr struct {
	Sexpr types.Sexpr
}

func (t Sexpr) String() string {
	return fmt.Sprintf("%s", t.Sexpr)
}

func (t Sexpr) Transform(m mapping.Mapping) types.Sexpr {
	switch sexpr := t.Sexpr.(type) {
	case types.Symbol:
		return transformSymbol(sexpr, m)
	case *types.Pair:
		return transformPair(sexpr, m)
	default:
		return sexpr
	}
}

func transformSymbol(s types.Symbol, m mapping.Mapping) types.Sexpr {
	if val, ok := m[s]; ok {
		return val
	}
	return s
}

func transformPair(p *types.Pair, m mapping.Mapping) *types.Pair {
	ap := types.NewAppendablePair()
	head := p
	for head != nil {
		switch obj := head.This.(type) {
		case Ellipsis:
			val := obj.Transform(m)
			ap.Extend(val)
		case types.Symbol:
			val := transformSymbol(obj, m)
			ap.Append(val)
		case *types.Pair:
			val := transformPair(obj, m)
			ap.Append(val)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair()
}
