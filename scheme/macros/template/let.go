package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LetTemplate)(nil)
var _ Template = (*LetStarTemplate)(nil)

type LetTemplate struct {
	Binings *types.Pair
	Body    *types.Pair
}

func (t LetTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("let")

	local := m.Copy()
	args, err := transformBindings(t.Binings, m, local)
	if err != nil {
		return nil, err
	}
	ap.Append(args)

	body, err := transformPair(t.Body, local)
	if err != nil {
		return nil, err
	}
	ap.Extend(body)

	return ap.ToPair(), nil
}

func transformBindings(pair *types.Pair, parent, local *MappingIterator) (*types.Pair, error) {
	var (
		val      types.Sexpr
		bindings = types.NewAppendablePair()
		head     = pair
		err      error
	)
	for head != nil {
		switch binding := head.This.(type) {
		case *types.Pair:
			val, err = transformBinding(binding, parent, local)
			if err != nil {
				return nil, err
			}
		default:
			val, err = Transform(head.This, parent)
			if err != nil {
				return nil, err
			}
		}
		bindings.Append(val)
		head = head.Next
	}
	return bindings.ToPair(), nil
}

func transformBinding(binding *types.Pair, parent, local *MappingIterator) (types.Sexpr, error) {
	if binding == nil || binding.IsNull() || !binding.HasNext() {
		return transformPair(binding, parent)
	}

	switch sym := binding.This.(type) {
	case types.Symbol:
		// first transform the value, so not to shawdow it accidentally as in (let ((x x)) ...)
		val, err := Transform(binding.Next.This, parent)
		if err != nil {
			return nil, err
		}

		// transform the key
		name := gensym.Generator.New()
		local.Set(sym, name)

		return types.NewPair(name, val), nil
	default:
		return binding, fmt.Errorf("%s is not a valid binding", binding)
	}
}

// (let ((binding value) ...) ...) body ...)
func parseLet(args *types.Pair) (LetTemplate, error) {
	if args == nil || !args.HasNext() {
		return LetTemplate{}, eval.ErrBadArgNumber
	}
	switch obj := args.This.(type) {
	case *types.Pair:
		bindings, err := parseAll(obj)
		if err != nil {
			return LetTemplate{}, err
		}
		body, err := parseAll(args.Next)
		if err != nil {
			return LetTemplate{}, err
		}
		return LetTemplate{bindings, body}, nil
	default:
		return LetTemplate{}, eval.NewErrNonList(args.This)
	}
}

func (t LetTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("let")
	ap.Append(t.Binings)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}

type LetStarTemplate struct {
	Binings *types.Pair
	Body    *types.Pair
}

func (t LetStarTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append("let*")

	args, err := transformBindings(t.Binings, m, m)
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

// (let* ((binding value) ...) ...) body ...)
func parseLetStar(args *types.Pair) (LetStarTemplate, error) {
	if args == nil || !args.HasNext() {
		return LetStarTemplate{}, eval.ErrBadArgNumber
	}
	switch obj := args.This.(type) {
	case *types.Pair:
		bindings, err := parseAll(obj)
		if err != nil {
			return LetStarTemplate{}, err
		}
		body, err := parseAll(args.Next)
		if err != nil {
			return LetStarTemplate{}, err
		}
		return LetStarTemplate{bindings, body}, nil
	default:
		return LetStarTemplate{}, eval.NewErrNonList(args.This)
	}
}

func (t LetStarTemplate) String() string {
	ap := types.NewAppendablePair()
	ap.Append("let*")
	ap.Append(t.Binings)
	ap.Extend(t.Body)
	return fmt.Sprintf("%v", ap.ToPair())
}
