package macros

import "github.com/twolodzko/gosch/types"

type LetTransformer struct {
	Transformer
}

func newLetTransformer(transformer Transformer) LetTransformer {
	transformer.level += 1
	return LetTransformer{transformer}
}

// (let ((binding value) ...) ...) body ...)
func (t *LetTransformer) parse(args *types.Pair) (*types.Pair, *types.Pair, bool) {
	if args == nil || !args.HasNext() {
		return &types.Pair{}, &types.Pair{}, false
	}
	switch params := args.This.(type) {
	case *types.Pair:
		return params, args.Next, true
	default:
		return &types.Pair{}, &types.Pair{}, false
	}
}

func (t *LetTransformer) transform(bindings, body *types.Pair) *types.Pair {
	ast := types.NewAppendablePair()
	ast.Append(types.Symbol("let"))
	ast.Append(t.transformBindings(bindings))
	ast.Extend(t.transformAll(body))
	return ast.ToPair()
}

func (t *LetTransformer) transformBindings(pair *types.Pair) *types.Pair {
	bindings := types.NewAppendablePair()
	head := pair
	for head != nil {
		var val types.Sexpr
		switch binding := head.This.(type) {
		case *types.Pair:
			val = t.transformBinding(binding)
		default:
			val = t.transformSexpr(head.This)
		}
		bindings.Append(val)
		head = head.Next
	}
	return bindings.ToPair()
}

func (t *LetTransformer) transformBinding(binding *types.Pair) types.Sexpr {
	if binding == nil || binding.IsNull() || !binding.HasNext() {
		return t.transformPair(binding)
	}

	switch sym := binding.This.(type) {
	case types.Symbol:
		// first transform the binding, so not to shawdow it accidentally as in (let ((x x)) ...)
		var val types.Sexpr
		if obj, ok := t.mappings[sym]; ok {
			val = obj
		} else {
			val = sym
		}

		name := t.Rename(sym)
		t.mappings[sym] = name

		return types.NewPair(name, val)
	default:
		// FIXME: error here
		return t.transformPair(binding)
	}
}
