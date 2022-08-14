package macros

import "github.com/twolodzko/gosch/types"

type LambdaTransformer struct {
	Transformer
}

func newLambdaTransformer(transformer Transformer) LambdaTransformer {
	transformer.level += 1
	return LambdaTransformer{transformer}
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

func (t *LambdaTransformer) transform(args, body *types.Pair) (*types.Pair, error) {
	ast := types.NewAppendablePair()
	ast.Append(types.Symbol("lambda"))

	var err error
	args, err = t.transformArgs(args)
	if err != nil {
		return nil, err
	}
	ast.Append(args)

	body, err = t.transformAll(body)
	if err != nil {
		return nil, err
	}
	ast.Extend(body)

	return ast.ToPair(), nil
}

func (t *LambdaTransformer) transformArgs(pair *types.Pair) (*types.Pair, error) {
	args := types.NewAppendablePair()
	head := pair
	for head != nil {
		switch sym := head.This.(type) {
		case types.Symbol:
			name := t.Rename(sym)
			t.mappings[sym] = name
			args.Append(name)
		default:
			val, err := t.transformSexpr(head.This)
			if err != nil {
				return nil, err
			}
			args.Append(val)
		}
		head = head.Next
	}
	return args.ToPair(), nil
}
