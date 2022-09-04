package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LispMacroTemplate)(nil)

type LispMacroTemplate struct {
	Args *types.Pair
	Body *types.Pair
}

func (t LispMacroTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("macro")

	args, err := transformArgs(t.Args, m)
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

// (macro (args ...) body ...)
func parseLispMacro(args *types.Pair) (LispMacroTemplate, error) {
	if args == nil || !args.HasNext() {
		return LispMacroTemplate{}, eval.ErrBadArgNumber
	}
	switch obj := args.This.(type) {
	case *types.Pair:
		params, err := parseAll(obj)
		if err != nil {
			return LispMacroTemplate{}, err
		}
		body, err := parseAll(args.Next)
		if err != nil {
			return LispMacroTemplate{}, err
		}
		return LispMacroTemplate{params, body}, nil
	default:
		return LispMacroTemplate{}, eval.NewErrNonList(args.This)
	}
}

func (t LispMacroTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("macro")
	ap.Append(t.Args)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}
