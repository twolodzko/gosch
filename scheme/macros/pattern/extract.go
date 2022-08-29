package pattern

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

const (
	Self     = types.Symbol("_")
	Ellipsis = types.Symbol("...")
)

func Extract(pair *types.Pair, literals []types.Symbol) (*Pattern, error) {
	p, err := fromPair(pair, literals)
	pattern := Pattern(p)
	return &pattern, err
}

func fromPair(pair *types.Pair, literals []types.Symbol) ([]Subpattern, error) {
	var patterns []Subpattern

	if pair == nil || pair.IsNull() {
		return patterns, nil
	}

	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			if obj == Ellipsis {
				if len(patterns) == 0 {
					return nil, fmt.Errorf("%s needs to be preceeded with a pattern", Ellipsis)
				}
				if head.HasNext() {
					return nil, fmt.Errorf("nothing should follow after %s", Ellipsis)
				}
				patterns[len(patterns)-1].ToEllipsis()
			} else {
				p := fromSymbol(obj, literals)
				patterns = append(patterns, p)
			}
		case *types.Pair:
			matched, err := fromPair(obj, literals)
			if err != nil {
				return nil, err
			}
			var pair *Pair
			if len(matched) > 0 {
				pair = &Pair{matched, false}
			} else {
				pair = &Pair{}
			}
			patterns = append(patterns, pair)
		default:
			p := &Literal{obj}
			patterns = append(patterns, p)
		}
		head = head.Next
	}

	return patterns, nil
}

func fromSymbol(obj string, literals []string) Subpattern {
	switch {
	case isLiteral(obj, literals):
		return &Literal{obj}
	default:
		return &Identifier{obj, false}
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
