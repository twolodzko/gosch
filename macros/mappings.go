package macros

import (
	"github.com/twolodzko/gosch/types"
)

type Mappings map[types.Symbol]types.Sexpr

func (x *Mappings) merge(y Mappings) bool {
	for key, val := range y {
		if _, ok := (*x)[key]; ok {
			return false
		}
		(*x)[key] = val
	}
	return true
}
