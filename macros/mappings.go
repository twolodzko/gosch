package macros

import (
	"github.com/twolodzko/gosch/types"
)

type Mappings map[types.Symbol]types.Sexpr

func mergeMappings(x, y Mappings) (Mappings, bool) {
	for key, val := range y {
		if _, ok := x[key]; ok {
			return Mappings{}, false
		}
		x[key] = val
	}
	return x, true
}
