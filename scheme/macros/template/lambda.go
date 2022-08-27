package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
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

	args, err := transformPair(t.Args, m)
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
