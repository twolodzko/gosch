package template

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LispMacroTemplate)(nil)

type LispMacroTemplate types.Pair

func (t LispMacroTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
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

// (macro (args ...) body ...)
func newLispMacro(args *types.Pair) (LispMacroTemplate, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return LispMacroTemplate{}, &ErrInvalidTemplate{args}
	}
	return LispMacroTemplate(*args), nil
}

func (t LispMacroTemplate) String() string {
	return fmt.Sprintf("%v", types.Pair(t))
}
