package mapping

import (
	"github.com/twolodzko/gosch/types"
)

type Mapping map[types.Symbol]types.Sexpr

func (x *Mapping) Merge(y Mapping) bool {
	for key, val := range y {
		if _, ok := (*x)[key]; ok {
			return false
		}
		(*x)[key] = val
	}
	return true
}

func (m Mapping) Copy() Mapping {
	m2 := make(Mapping)
	for key, val := range m {
		m2[key] = val
	}
	return m2
}

func (x Mapping) Extend(y Mapping) {
	for key, yval := range y {
		if xval, ok := x[key]; ok {
			switch xval := xval.(type) {
			case Ellipsis:
				x[key] = append(xval, yval)
			default:
				e := Ellipsis{xval}
				x[key] = append(e, yval)
			}
		} else {
			x[key] = Ellipsis{yval}
		}
	}
}
