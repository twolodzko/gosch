package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LambdaTemplate)(nil)

type LambdaTemplate types.Pair

func (t LambdaTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append(t.This)

	switch obj := (t.Next.This).(type) {
	case *types.Pair:
		args, err := transformArgs(obj, m)
		if err != nil {
			return nil, err
		}
		ap.Append(args)
	default:
		return nil, &ErrInvalidTemplate{t}
	}

	body, err := transformPair(t.Next.Next, m)
	if err != nil {
		return nil, err
	}
	ap.Extend(body)

	return ap.ToPair(), nil
}

func transformArgs(args *types.Pair, m *MappingIterator) (*types.Pair, error) {
	ap := types.NewAppendablePair()
	head := args
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			val := gensym.Generator.New()
			m.Set(obj, val)
			ap.Append(val)
		case Ellipsis:
			val, err := obj.Transform(m)
			if err != nil {
				return nil, err
			}
			ap.Extend(val)
		default:
			return nil, eval.NewErrBadName(head.This)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}

// (lambda (args ...) body ...)
func newLambda(args *types.Pair) (LambdaTemplate, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return LambdaTemplate{}, &ErrInvalidTemplate{args}
	}
	obj, err := parseAll(args)
	if err != nil {
		return LambdaTemplate{}, err
	}
	return LambdaTemplate(*obj), nil
}

func (t LambdaTemplate) String() string {
	return fmt.Sprintf("%v", types.Pair(t))
}
