package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type Transformer struct {
	mappings Mappings
	env      *envir.Env
	level    int
}

func newTransformer(mappings Mappings, env *envir.Env) Transformer {
	return Transformer{mappings, env, 0}
}

func (t *Transformer) transform(template types.Sexpr) (types.Sexpr, error) {
	return t.transformSexpr(template)
}

func (t *Transformer) transformSexpr(sexpr types.Sexpr) (types.Sexpr, error) {
	switch obj := sexpr.(type) {
	case types.Symbol:
		return t.transformSymbol(obj), nil
	case *types.Pair:
		return t.transformPair(obj)
	default:
		return obj, nil
	}
}

func (t *Transformer) transformSymbol(symbol types.Symbol) types.Sexpr {
	if val, ok := t.mappings[symbol]; ok {
		return val
	}
	return symbol
}

func (t *Transformer) transformPair(pair *types.Pair) (types.Sexpr, error) {
	if pair == nil || pair.IsNull() {
		return pair, nil
	}

	if name, ok := pair.This.(types.Symbol); ok {
		switch name {
		case "lambda":
			transformer := newLambdaTransformer(*t)
			if args, body, ok := transformer.parse(pair.Next); ok {
				return transformer.transform(args, body)
			}
		case "let":
			transformer := newLetTransformer(*t)
			if bindings, body, ok := transformer.parse(pair.Next); ok {
				return transformer.transform(bindings, body)
			}
		case "do":
			// FIXME
		}

		if obj, ok := t.env.Get(name); ok {
			if rules, ok := obj.(SyntaxRules); ok {
				args, err := t.transformAll(pair.Next)
				if err != nil {
					return nil, err
				}
				if sexpr, _, err := rules.Call(args, t.env); ok {
					return sexpr, err
				}
			}
		}
	}
	return t.transformAll(pair)
}

func (t *Transformer) transformAll(pair *types.Pair) (*types.Pair, error) {
	body := types.NewAppendablePair()
	head := pair
	for head != nil {
		if head.This == Ellipsis {
			val, err := t.getEllipsis()
			if err != nil {
				return nil, err
			}
			body.Extend(val)
		} else {
			val, err := t.transformSexpr(head.This)
			if err != nil {
				return nil, err
			}
			body.Append(val)
		}
		head = head.Next
	}
	return body.ToPair(), nil
}

func (t *Transformer) getEllipsis() (*types.Pair, error) {
	if ellipsis, ok := t.mappings[Ellipsis]; ok {
		return t.transformAll(ellipsis.(*types.Pair))
	}
	return &types.Pair{}, nil
}

func (t Transformer) Rename(name types.Symbol) types.Symbol {
	return fmt.Sprintf("%s_%d", name, t.level)
}

// type Counter map[types.Symbol]int

// func (c *Counter) Add(key types.Symbol) {
// 	(*c)[key] = c.Get(key) + 1
// }

// func (c Counter) Get(key types.Symbol) int {
// 	if count, ok := c[key]; ok {
// 		return count
// 	} else {
// 		return 0
// 	}
// }
