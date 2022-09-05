package pattern

import (
	"github.com/twolodzko/gosch/types"
)

type EllipsisVar []types.Sexpr

func (e EllipsisVar) ToPair() *types.Pair {
	return types.NewPair(e...)
}

func (e EllipsisVar) IsNested() bool {
	if len(e) == 0 {
		return false
	}
	_, ok := e[0].(EllipsisVar)
	return ok
}

func ellipsisVarFromPair(pair *types.Pair) EllipsisVar {
	if pair == nil || pair.IsNull() {
		return EllipsisVar{}
	}

	var e EllipsisVar
	head := pair
	for head != nil && !head.IsNull() {
		e = append(e, head.This)
		head = head.Next
	}
	return e
}
