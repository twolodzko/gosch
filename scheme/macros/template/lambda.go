package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LambdaTemplate)(nil)

type LambdaTemplate struct {
	Args *types.Pair
	Body *types.Pair
}

func (t LambdaTemplate) Transform(m mapping.Mapping) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("lambda")

	args, err := t.transformArgs(m)
	if err != nil {
		return nil, err
	}
	ap.Append(args)

	body, err := transformPair(t.Body, m)
	if err != nil {
		return nil, err
	}
	ap.Extend(body)

	return ap.ToPair(), nil
}

func (t LambdaTemplate) transformArgs(m mapping.Mapping) (*types.Pair, error) {
	ap := types.NewAppendablePair()
	head := t.Args
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			val := expandOrRenameSymbol(obj, m)
			ap.Append(val)
		case Ellipsis:
			val := obj.Transform(m)
			ap.Extend(val)
		default:
			return nil, eval.NewErrBadName(head.This)
		}
		head = head.Next
	}
	return ap.ToPair(), nil
}

// (lambda (args ...) body ...)
func parseLambda(args *types.Pair) (LambdaTemplate, error) {
	if args == nil || !args.HasNext() {
		return LambdaTemplate{}, eval.ErrBadArgNumber
	}
	switch obj := args.This.(type) {
	case *types.Pair:
		params, err := parseAll(obj)
		if err != nil {
			return LambdaTemplate{}, err
		}
		body, err := parseAll(args.Next)
		if err != nil {
			return LambdaTemplate{}, err
		}
		return LambdaTemplate{params, body}, nil
	default:
		return LambdaTemplate{}, eval.NewErrNonList(args.This)
	}
}

func (t LambdaTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("lambda")
	ap.Append(t.Args)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}

func expandOrRenameSymbol(s types.Symbol, m mapping.Mapping) types.Sexpr {
	if val, ok := m[s]; ok {
		return val
	}
	val := gensym.Generator.New()
	m[s] = val
	return val
}
