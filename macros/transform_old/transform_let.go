package macros

// import (
// 	"fmt"

// 	"github.com/twolodzko/gosch/types"
// )

// type LetTransformer struct {
// 	Transformer
// }

// func newLetTransformer(transformer Transformer) LetTransformer {
// 	transformer.level += 1
// 	return LetTransformer{transformer}
// }

// // (let ((binding value) ...) ...) body ...)
// func (t *LetTransformer) parse(args *types.Pair) (*types.Pair, *types.Pair, bool) {
// 	if args == nil || !args.HasNext() {
// 		return &types.Pair{}, &types.Pair{}, false
// 	}
// 	switch params := args.This.(type) {
// 	case *types.Pair:
// 		return params, args.Next, true
// 	default:
// 		return &types.Pair{}, &types.Pair{}, false
// 	}
// }

// func (t *LetTransformer) transform(bindings, body *types.Pair) (*types.Pair, error) {
// 	ast := types.NewAppendablePair()
// 	ast.Append(types.Symbol("let"))

// 	var err error
// 	bindings, err = t.transformBindings(bindings)
// 	if err != nil {
// 		return nil, err
// 	}
// 	ast.Append(bindings)

// 	body, err = t.transformAll(body)
// 	if err != nil {
// 		return nil, err
// 	}
// 	ast.Extend(body)

// 	return ast.ToPair(), nil
// }

// func (t *LetTransformer) transformBindings(pair *types.Pair) (*types.Pair, error) {
// 	var (
// 		val types.Sexpr
// 		err error
// 	)
// 	bindings := types.NewAppendablePair()
// 	head := pair
// 	for head != nil {
// 		switch binding := head.This.(type) {
// 		case *types.Pair:
// 			val, err = t.transformBinding(binding)
// 			if err != nil {
// 				return nil, err
// 			}
// 		default:
// 			val, err = t.transformSexpr(head.This)
// 			if err != nil {
// 				return nil, err
// 			}
// 		}
// 		bindings.Append(val)
// 		head = head.Next
// 	}
// 	return bindings.ToPair(), nil
// }

// func (t *LetTransformer) transformBinding(binding *types.Pair) (types.Sexpr, error) {
// 	if binding == nil || binding.IsNull() || !binding.HasNext() {
// 		return t.transformPair(binding)
// 	}

// 	switch sym := binding.This.(type) {
// 	case types.Symbol:
// 		// first transform the value, so not to shawdow it accidentally as in (let ((x x)) ...)
// 		val, err := t.transformSexpr(binding.Next.This)
// 		if err != nil {
// 			return nil, err
// 		}

// 		// transform the key
// 		name := t.Rename(sym)
// 		t.mappings[sym] = name

// 		return types.NewPair(name, val), nil
// 	default:
// 		return nil, fmt.Errorf("%v is not a valid binding", binding.This)
// 	}
// }
