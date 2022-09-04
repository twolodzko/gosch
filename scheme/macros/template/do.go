package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*DoTemplate)(nil)

type DoTemplate struct {
	Bindings *types.Pair
	Body     *types.Pair
}

func (t DoTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("do")

	args, err := transformDoBindings(t.Bindings, m)
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
func parseDo(args *types.Pair) (DoTemplate, error) {
	if args == nil || !args.HasNext() {
		return DoTemplate{}, eval.ErrBadArgNumber
	}
	switch obj := args.This.(type) {
	case *types.Pair:
		bindings, err := parseAll(obj)
		if err != nil {
			return DoTemplate{}, err
		}
		body, err := parseAll(args.Next)
		if err != nil {
			return DoTemplate{}, err
		}
		return DoTemplate{bindings, body}, nil
	default:
		return DoTemplate{}, eval.NewErrNonList(args.This)
	}
}

func (t DoTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("do")
	ap.Append(t.Bindings)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}
