package macros

import (
	"github.com/twolodzko/gosch/types"
)

const (
	Self     = types.Symbol("_")
	Ellipsis = types.Symbol("...")
)

func patternFromPair(pair *types.Pair, literals []types.Symbol) PairPattern {
	var (
		pattern  Pattern
		patterns []Pattern
	)

	if pair == nil || pair.IsNull() {
		return PairPattern{}
	}

	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			pattern = patternFromSymbol(obj, literals)
		case *types.Pair:
			pattern = patternFromPair(obj, literals)
		default:
			pattern = LiteralPattern{obj}
		}
		patterns = append(patterns, pattern)
		head = head.Next
	}

	return PairPattern{patterns}
}

func patternFromSymbol(obj string, literals []string) Pattern {
	switch {
	case isLiteral(obj, literals):
		return LiteralPattern{obj}
	case obj == Ellipsis:
		return EllipsisPattern{}
	default:
		return IdentifierPattern{obj}
	}
}

func isLiteral(obj types.Symbol, literals []types.Symbol) bool {
	for _, l := range literals {
		if obj == l {
			return true
		}
	}
	return false
}
