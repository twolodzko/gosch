package macros

import "github.com/twolodzko/gosch/types"

type LambdaTransformer struct {
	Transformer
	suffix string
}

func newLambdaTransformer(mappings Mappings) LambdaTransformer {
	suffix := newSuffix()
	return LambdaTransformer{Transformer{mappings}, suffix}
}

// (lambda (args ...) body ...)
func (t LambdaTransformer) parse(args *types.Pair) (*types.Pair, *types.Pair, bool) {
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

func (t *LambdaTransformer) transform(args, body *types.Pair) *types.Pair {
	ast := types.NewAppendablePair()
	ast.Append(types.Symbol("lambda"))
	ast.Append(t.transformArgs(args))
	ast.Extend(t.transformAll(body))
	return ast.ToPair()
}

func (t *LambdaTransformer) transformArgs(pair *types.Pair) *types.Pair {
	args := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch sym := head.This.(type) {
		case types.Symbol:
			name := newName(sym, t.suffix)
			t.mappings[sym] = name
			args.Append(name)
		default:
			val := t.transformSexpr(head.This)
			args.Append(val)
		}
		head = head.Next
	}
	return args.ToPair()
}
