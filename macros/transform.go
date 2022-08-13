package macros

import (
	"fmt"
	"math/big"
	"time"

	"github.com/twolodzko/gosch/types"
)

type Transformer struct {
	mappings Mappings
}

func newTransformer(mappings Mappings) Transformer {
	return Transformer{mappings}
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
			transformer := newLambdaTransformer(t.mappings)
			if args, body, ok := transformer.parse(pair.Next); ok {
				return transformer.transform(args, body)
			}
		case "let":
			transformer := newLetTransformer(t.mappings)
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
	ellipsis := t.mappings[Ellipsis].(*types.Pair)
	return t.transformAll(ellipsis)
}

// Split Unix timestamp into three blocks, XOR them together, and convert to base 62
func newSuffix() string {
	var (
		h int64 = 0
		x int64
		m int64 = 10_000_000
	)
	n := time.Now().UnixNano()
	for n > 0 {
		x = n % m
		h ^= x
		n = n / m
	}
	return big.NewInt(h).Text(62)
}

func newName(symbol types.Symbol, suffix string) types.Symbol {
	return fmt.Sprintf("%s:%s", symbol, suffix)
}
