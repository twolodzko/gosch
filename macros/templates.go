package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

type Template interface {
	Transform(Mappings) (types.Sexpr, error)
}

// type LambdaTemplate struct {
// 	Args *types.Pair
// 	Body *types.Pair
// }

// type LetTemplate struct {
// 	Binings *types.Pair
// 	Body    *types.Pair
// }

type SexprTemplate struct {
	Sexpr types.Sexpr
}

func (t SexprTemplate) String() string {
	return fmt.Sprintf("%s", t.Sexpr)
}

func (t SexprTemplate) Transform(m Mappings) (types.Sexpr, error) {
	switch sexpr := t.Sexpr.(type) {
	case types.Symbol:
		return transformSymbol(sexpr, m), nil
	case *types.Pair:
		return transformPair(sexpr, m)
	default:
		return sexpr, nil
	}
}

func transformSymbol(s types.Symbol, m Mappings) types.Sexpr {
	if val, ok := m[s]; ok {
		return val
	}
	return s
}

func transformPair(p *types.Pair, m Mappings) (*types.Pair, error) {
	ap := types.NewAppendablePair()
	head := p
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			val := transformSymbol(obj, m)
			ap.Append(val)
		case *types.Pair:
			val, err := transformPair(obj, m)
			if err != nil {
				return nil, err
			}
			ap.Append(val)
		case EllipsisTemplate:
			val, err := obj.Transform(m)
			if err != nil {
				return nil, err
			}
			ap.Extend(val)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}

type EllipsisTemplate struct {
	Sexpr types.Sexpr
	Vars  []types.Symbol
}

func (t EllipsisTemplate) Transform(m Mappings) (*types.Pair, error) {
	var (
		ap       = types.NewAppendablePair()
		template = SexprTemplate{t.Sexpr}
		mapping  = make(Mappings)
		ellipses = make(map[types.Sexpr]EllipsisVar)
	)

	for key, val := range m {
		mapping[key] = val
	}

	if len(t.Vars) == 0 {
		return nil, fmt.Errorf("variables in ellipsis cannot be empty")
	}

	for _, key := range t.Vars {
		switch val := m[key].(type) {
		case EllipsisVar:
			ellipses[key] = val
		default:
			return nil, fmt.Errorf("%s is not a valid variable", val)
		}
	}

	i := 0
	for {
		for _, key := range t.Vars {
			if i >= len(ellipses[key]) {
				return ap.ToPair(), nil
			}
			mapping[key] = ellipses[key][i]
		}
		val, err := template.Transform(mapping)
		if err != nil {
			return nil, err
		}
		ap.Append(val)
		i++
	}
}

func (t EllipsisTemplate) String() string {
	return fmt.Sprintf("%s ...", t.Sexpr)
}
