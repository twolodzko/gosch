package template

import (
	"fmt"

	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*DoTemplate)(nil)

type DoTemplate types.Pair

func (t DoTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append(t.This)

	switch obj := (t.Next.This).(type) {
	case *types.Pair:
		args, err := transformDoBindings(obj, m)
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

func transformDoBindings(pair *types.Pair, m *MappingIterator) (*types.Pair, error) {
	var (
		val      types.Sexpr
		bindings = types.NewAppendablePair()
		head     = pair
		err      error
	)
	for head != nil {
		switch binding := head.This.(type) {
		case *types.Pair:
			val, err = transformDoBinding(binding, m)
			if err != nil {
				return nil, err
			}
		default:
			val, err = Transform(head.This, m)
			if err != nil {
				return nil, err
			}
		}
		bindings.Append(val)
		head = head.Next
	}
	return bindings.ToPair(), nil
}

func transformDoBinding(binding *types.Pair, m *MappingIterator) (types.Sexpr, error) {
	if binding == nil || binding.IsNull() || !binding.HasNext() {
		return transformPair(binding, m)
	}

	switch sym := binding.This.(type) {
	case types.Symbol:
		name := gensym.Generator.New()
		m.Set(sym, name)

		val, err := transformPair(binding.Next, m)
		if err != nil {
			return nil, err
		}

		return types.MakePair(name, val), nil
	default:
		return binding, fmt.Errorf("%s is not a valid binding", binding)
	}
}

// (do ((var init update) ...) (test result ...) expr ...)
func newDo(args *types.Pair) (DoTemplate, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return DoTemplate{}, &ErrInvalidTemplate{args}
	}
	obj, err := parseAll(args)
	if err != nil {
		return DoTemplate{}, err
	}
	return DoTemplate(*obj), nil
}

func (t DoTemplate) String() string {
	return fmt.Sprintf("%v", types.Pair(t))
}
