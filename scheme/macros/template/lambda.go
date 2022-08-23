package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

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

	body := transformPair(t.Body, m)
	ap.Extend(body)

	return ap.ToPair(), nil
}

func (t LambdaTemplate) transformArgs(m mapping.Mapping) (*types.Pair, error) {
	args := types.NewAppendablePair()
	head := t.Args
	for head != nil {
		switch sym := head.This.(type) {
		case types.Symbol:
			name := gensym.Generator.New()
			m[sym] = name
			args.Append(name)
		default:
			val, err := Transform(head.This, m)
			if err != nil {
				return nil, err
			}
			args.Append(val)
		}
		head = head.Next
	}
	return args.ToPair(), nil
}

// (lambda (args ...) body ...)
func parseLambda(args *types.Pair) (LambdaTemplate, bool) {
	if args == nil || !args.HasNext() {
		return LambdaTemplate{}, false
	}
	switch params := args.This.(type) {
	case *types.Pair:
		return LambdaTemplate{params, args.Next}, true
	default:
		return LambdaTemplate{}, false
	}
}

func (t LambdaTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("lambda")
	ap.Append(t.Args)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}
