package template

import (
	"fmt"

	"github.com/twolodzko/gosch/macros/gensym"
	"github.com/twolodzko/gosch/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

type LetTemplate struct {
	Binings *types.Pair
	Body    *types.Pair
}

func (t LetTemplate) Transform(m mapping.Mapping) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("let")

	args, err := t.transformBindings(m)
	if err != nil {
		return nil, err
	}
	ap.Append(args)

	body := transformPair(t.Body, m)
	ap.Extend(body)

	return ap.ToPair(), nil
}

func (t *LetTemplate) transformBindings(m mapping.Mapping) (*types.Pair, error) {
	var (
		val      types.Sexpr
		bindings = types.NewAppendablePair()
		head     = t.Binings
		err      error
	)
	for head != nil {
		switch binding := head.This.(type) {
		case *types.Pair:
			val, err = t.transformBinding(binding, m)
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

func (t *LetTemplate) transformBinding(binding *types.Pair, m mapping.Mapping) (types.Sexpr, error) {
	if binding == nil || binding.IsNull() || !binding.HasNext() {
		return transformPair(binding, m), nil
	}

	switch sym := binding.This.(type) {
	case types.Symbol:
		// first transform the value, so not to shawdow it accidentally as in (let ((x x)) ...)
		val, err := Transform(binding.Next.This, m)
		if err != nil {
			return nil, err
		}

		// transform the key
		name := gensym.Generator.New()
		m[sym] = name

		return types.NewPair(name, val), nil
	default:
		return binding, fmt.Errorf("%s is not a valid binding", binding)
	}
}

// (let ((binding value) ...) ...) body ...)
func parseLet(args *types.Pair) (LetTemplate, bool) {
	if args == nil || !args.HasNext() {
		return LetTemplate{}, false
	}
	switch params := args.This.(type) {
	case *types.Pair:
		return LetTemplate{params, args.Next}, true
	default:
		return LetTemplate{}, false
	}
}

func (t LetTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("let")
	ap.Append(t.Binings)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}
