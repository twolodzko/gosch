package mapping

import (
	"github.com/twolodzko/gosch/types"
)

type Ellipsis []types.Sexpr

func (e Ellipsis) ToPair() *types.Pair {
	return types.NewPair(e...)
}

func (e Ellipsis) ContainsEllipses() bool {
	if len(e) == 0 {
		return false
	}
	_, ok := e[0].(Ellipsis)
	return ok
}

func EllipsisFromPair(pair *types.Pair) Ellipsis {
	if pair == nil || pair.IsNull() {
		return Ellipsis{}
	}

	var e Ellipsis
	head := pair
	for head != nil && !head.IsNull() {
		e = append(e, head.This)
		head = head.Next
	}
	return e
}
