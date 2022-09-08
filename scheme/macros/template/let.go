package template

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

var _ Template = (*LetTemplate)(nil)
var _ Template = (*LetStarTemplate)(nil)

type LetTemplate types.Pair

func (t LetTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	switch obj := (t.Next.This).(type) {
	case *types.Pair:
		return regularLet(t, m)
	case types.Symbol:
		return namedLet(obj, t, m)
	default:
		return nil, &ErrInvalidTemplate{t}
	}
}

func regularLet(t LetTemplate, m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append(t.This)

	local := m.Copy()
	bindings, body, err := transformLet(t.Next, m, local)
	if err != nil {
		return nil, err
	}
	ap.Append(bindings)
	ap.Extend(body)

	return ap.ToPair(), nil
}

func namedLet(sym types.Symbol, t LetTemplate, m *MappingIterator) (types.Sexpr, error) {
	if !t.Next.HasNext() {
		return nil, &ErrInvalidTemplate{t}
	}

	ap := types.NewAppendablePair()
	ap.Append(t.This)

	local := m.Copy()

	name := gensym.Generator.New()
	local.Set(sym, name)
	ap.Append(name)

	bindings, body, err := transformLet(t.Next.Next, m, local)
	if err != nil {
		return nil, err
	}
	ap.Append(bindings)
	ap.Extend(body)

	return ap.ToPair(), nil
}

func transformLet(sexpr *types.Pair, parent, local *MappingIterator) (*types.Pair, *types.Pair, error) {
	pair, ok := sexpr.This.(*types.Pair)
	if !ok {
		return nil, nil, eval.NewErrNonList(sexpr.This)
	}
	bindings, err := transformBindings(pair, parent, local)
	if err != nil {
		return nil, nil, err
	}
	body, err := transformPair(sexpr.Next, local)
	if err != nil {
		return nil, nil, err
	}
	return bindings, body, nil
}

func transformBindings(pair *types.Pair, parent, local *MappingIterator) (*types.Pair, error) {
	var (
		bindings = types.NewAppendablePair()
		head     = pair
		val      types.Sexpr
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
func newLet(args *types.Pair) (LetTemplate, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return LetTemplate{}, &ErrInvalidTemplate{args}
	}
	return LetTemplate(*args), nil
}

func (t LetTemplate) String() string {
	return fmt.Sprintf("%v", types.Pair(t))
}

type LetStarTemplate types.Pair

func (t LetStarTemplate) Transform(m *MappingIterator) (types.Sexpr, error) {
	ap := types.NewAppendablePair()
	ap.Append(t.This)

	bindings, body, err := transformLet(t.Next, m, m)
	if err != nil {
		return nil, err
	}
	ap.Append(bindings)
	ap.Extend(body)

	return ap.ToPair(), nil
}

// (let* ((binding value) ...) ...) body ...)
func newLetStar(args *types.Pair) (LetStarTemplate, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return LetStarTemplate{}, &ErrInvalidTemplate{args}
	}
	return LetStarTemplate(*args), nil
}

func (t LetStarTemplate) String() string {
	return fmt.Sprintf("%v", types.Pair(t))
}
