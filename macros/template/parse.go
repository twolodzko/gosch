package template

import (
	"github.com/twolodzko/gosch/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

func Parse(sexpr types.Sexpr) (Sexpr, error) {
	val, err := parseSexpr(sexpr)
	return Sexpr{val}, err
}

func parseSexpr(sexpr types.Sexpr) (types.Sexpr, error) {
	switch sexpr := sexpr.(type) {
	case *types.Pair:
		return parsePair(sexpr)
	default:
		return sexpr, nil
	}
}

func parsePair(pair *types.Pair) (*types.Pair, error) {
	var (
		sexprs []types.Sexpr
		head   = pair
	)
	for head != nil {
		val, err := parseSexpr(head.This)
		if err != nil {
			return nil, err
		}

		if head.HasNext() && head.Next.This == pattern.Ellipsis {
			sexprs = append(sexprs, Ellipsis{val})
			head = head.Next
		} else {
			sexprs = append(sexprs, val)
		}

		head = head.Next
	}
	return types.PairFromArray(sexprs), nil
}
