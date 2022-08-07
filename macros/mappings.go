package macros

import "github.com/twolodzko/gosch/types"

type Mapping map[types.Symbol]types.Sexpr

func (m Mapping) Transform(template *types.Pair) *types.Pair {
	ap := types.NewAppendablePair()
	head := template
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			if replacement, ok := m[obj]; ok {
				ap.Append(replacement)
			} else {
				ap.Append(obj)
			}
		case *types.Pair:
			pair := m.Transform(obj)
			ap.Append(pair)
		default:
			ap.Append(obj)
		}
		head = head.Next
	}
	return ap.ToPair()
}

func mergeMappings(x, y Mapping) (Mapping, bool) {
	for key, val := range y {
		if _, ok := x[key]; ok {
			return Mapping{}, false
		}
		x[key] = val
	}
	return x, true
}
