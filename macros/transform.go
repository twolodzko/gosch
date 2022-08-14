package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

type Transformer struct {
	mappings Mappings
	level    int
}

func newTransformer(mappings Mappings) Transformer {
	return Transformer{mappings, 0}
}

func (t *Transformer) transform(template types.Sexpr) types.Sexpr {
	return t.transformSexpr(template)
}

func (t *Transformer) transformSexpr(sexpr types.Sexpr) types.Sexpr {
	switch obj := sexpr.(type) {
	case types.Symbol:
		return t.transformSymbol(obj)
	case *types.Pair:
		return t.transformPair(obj)
	default:
		return obj
	}
}

func (t *Transformer) transformSymbol(symbol types.Symbol) types.Sexpr {
	if val, ok := t.mappings[symbol]; ok {
		return val
	}
	return symbol
}

func (t *Transformer) transformPair(pair *types.Pair) *types.Pair {
	if pair == nil || pair.IsNull() {
		return pair
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
	}
	return t.transformAll(pair)
}

func (t *Transformer) transformAll(pair *types.Pair) *types.Pair {
	body := types.NewAppendablePair()
	head := pair
	for head != nil {
		if head.This == Ellipsis {
			val := t.getEllipsis()
			body.Extend(val)
		} else {
			val := t.transformSexpr(head.This)
			body.Append(val)
		}
		head = head.Next
	}
	return body.ToPair()
}

func (t *Transformer) getEllipsis() *types.Pair {
	if ellipsis, ok := t.mappings[Ellipsis]; ok {
		return t.transformAll(ellipsis.(*types.Pair))
	}
	return &types.Pair{}
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
