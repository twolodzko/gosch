package pattern

import "github.com/twolodzko/gosch/types"

type EllipsisVar []types.Sexpr

func (e EllipsisVar) ToPair() *types.Pair {
	return types.PairFromArray(e)
}

func ellipsisVarFromPair(pair *types.Pair) (EllipsisVar, bool) {
	if pair == nil || pair.IsNull() {
		return nil, false
	}

	var e EllipsisVar
	head := pair
	for head != nil {
		e = append(e, head.This)
		head = head.Next
	}
	return e, true
}
