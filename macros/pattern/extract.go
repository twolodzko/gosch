package pattern

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

const (
	Self     = types.Symbol("_")
	Ellipsis = types.Symbol("...")
)

func FromPair(pair *types.Pair, literals []types.Symbol) (*Pair, error) {
	var patterns []Pattern

	if pair == nil || pair.IsNull() {
		return &Pair{}, nil
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
			p, err := FromPair(obj, literals)
			if err != nil {
				return nil, err
			}
			patterns = append(patterns, p)
		default:
			p := &Literal{obj}
			patterns = append(patterns, p)
		}
		head = head.Next
	}

	return &Pair{patterns, false}, nil
}

func fromSymbol(obj string, literals []string) Pattern {
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
