package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

func Parse(sexpr types.Sexpr) (types.Sexpr, error) {
	switch sexpr := sexpr.(type) {
	case *types.Pair:
		return parsePair(sexpr)
	default:
		return sexpr, nil
	}
}

func parsePair(pair *types.Pair) (types.Sexpr, error) {
	var (
		sexprs []types.Sexpr
		head   = pair
	)

	switch pair.This {
	case "lambda":
		if template, ok := parseLambda(pair.Next); ok {
			return template, nil
		}
	case "let":
		if template, ok := parseLet(pair.Next); ok {
			return template, nil
		}
	case "let*":
		// FIXME: like `let`, but bindings need to depend on previous
	case "do":
		// FIXME
	case "macro":
		// FIXME
	}

	for head != nil {
		val, err := Parse(head.This)
		if err != nil {
			return nil, err
		}

		if head.HasNext() && head.Next.This == pattern.Ellipsis {
			val, err := toEllipsis(val)
			if err != nil {
				return nil, err
			}
			sexprs = append(sexprs, val)
			head = head.Next
		} else {
			sexprs = append(sexprs, val)
		}

		head = head.Next
	}
	return types.PairFromArray(sexprs), nil
}

func toEllipsis(sexpr types.Sexpr) (Ellipsis, error) {
	switch val := sexpr.(type) {
	case types.Symbol:
		return EllipsisSymbol(val), nil
	case *types.Pair:
		return EllipsisPair(*val), nil
	default:
		return nil, fmt.Errorf("%v is not a valid ellipsis", val)
	}
}
