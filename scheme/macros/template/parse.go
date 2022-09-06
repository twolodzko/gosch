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
	obj, err := parseAll(pair)
	if err != nil {
		return nil, err
	}

	switch pair.This {
	case "lambda":
		return newLambda(obj)
	case "let":
		return newLet(obj)
	case "let*":
		return newLetStar(obj)
	case "macro":
		return newLispMacro(obj)
	case "do":
		return newDo(obj)
	default:
		return obj, nil
	}
}

func parseAll(pair *types.Pair) (*types.Pair, error) {
	var (
		sexprs []types.Sexpr
		head   = pair
		val    types.Sexpr
		err    error
	)
	for head != nil {
		val, err = Parse(head.This)
		if err != nil {
			return nil, err
		}

		if head.HasNext() && head.Next.This == pattern.Ellipsis {
			val, err = toEllipsis(val)
			if err != nil {
				return nil, err
			}
			head = head.Next
		}

		sexprs = append(sexprs, val)
		head = head.Next
	}
	return types.NewPair(sexprs...), nil
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
