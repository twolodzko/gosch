package macros

import (
	"github.com/twolodzko/gosch/types"
)

const (
	Self     = types.Symbol("_")
	Ellipsis = types.Symbol("...")
)

func patternFromPair(pair *types.Pair, literals []types.Symbol) *PairPattern {
	var patterns []Pattern

	if pair == nil || pair.IsNull() {
		return &PairPattern{}
	}

	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			if obj == Ellipsis {
				// FIXME: handle errors: ellipsis is first
				patterns[len(patterns)-1].ToEllipsis()
			} else {
				p := patternFromSymbol(obj, literals)
				patterns = append(patterns, p)
			}
		case *types.Pair:
			p := patternFromPair(obj, literals)
			patterns = append(patterns, p)
		default:
			p := &LiteralPattern{obj}
			patterns = append(patterns, p)
		}
		head = head.Next
	}

	return &PairPattern{patterns, false}
}

func patternFromSymbol(obj string, literals []string) Pattern {
	switch {
	case isLiteral(obj, literals):
		return &LiteralPattern{obj}
	default:
		return &IdentifierPattern{obj, false}
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
