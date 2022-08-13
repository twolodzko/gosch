package macros

import (
	"fmt"
	"math/big"
	"time"

	"github.com/twolodzko/gosch/types"
)

type Renamings = Mappings

type Transformer struct {
	mappings Mappings
}

func newTransformer(mappings Mappings) Transformer {
	return Transformer{mappings}
}

func (t *Transformer) transformSexpr(template types.Sexpr) types.Sexpr {
	switch obj := template.(type) {
	case types.Symbol:
		return t.transformSymbol(obj)
	case *types.Pair:
		return t.transformPair(obj)
	default:
		return obj
	}
}

func (t *Transformer) transformSymbol(sym types.Symbol) types.Sexpr {
	if val, ok := t.mappings[sym]; ok {
		return val
	}
	return sym
}

func (t *Transformer) transformPair(template *types.Pair) *types.Pair {
	if template == nil || template.IsNull() {
		return template
	}
	if name, ok := template.This.(types.Symbol); ok {
		switch name {
		case "lambda":
			transformer := newLambdaTransformer(t.mappings)
			if args, body, ok := transformer.parse(template.Next); ok {
				return transformer.transform(args, body)
			}
		case "let":
			transformer := newLetTransformer(t.mappings)
			if bindings, body, ok := transformer.parse(template.Next); ok {
				return transformer.transform(bindings, body)
			}
		case "do":
			// FIXME
		}
	}
	return t.transformAll(template)
}

func (t *Transformer) transformAll(template *types.Pair) *types.Pair {
	ap := types.NewAppendablePair()
	head := template
	for head != nil {
		if head.This == Ellipsis {
			val := t.getEllipsis()
			ap.Extend(val)
		} else {
			val := t.transformSexpr(head.This)
			ap.Append(val)
		}
		head = head.Next
	}
	return ap.ToPair()
}

func (t *Transformer) getEllipsis() *types.Pair {
	ellipsis := t.mappings[Ellipsis].(*types.Pair)
	return t.transformAll(ellipsis)
}

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
	ast.Extend(t.transformBody(body))

	return ast.ToPair()
}

func (t *LambdaTransformer) transformArgs(sexpr *types.Pair) *types.Pair {
	args := types.NewAppendablePair()
	head := sexpr
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

func (t *LambdaTransformer) transformBody(sexpr *types.Pair) *types.Pair {
	ast := types.NewAppendablePair()
	head := sexpr
	for head != nil {
		var val types.Sexpr
		switch sym := head.This.(type) {
		case types.Symbol:
			val = t.transformSymbol(sym)
		default:
			val = t.transformSexpr(head.This)
		}
		ast.Append(val)
		head = head.Next
	}
	return ast.ToPair()
}

type LetTransformer struct {
	Transformer
	suffix string
}

func newLetTransformer(mappings Mappings) LetTransformer {
	suffix := newSuffix()
	return LetTransformer{Transformer{mappings}, suffix}
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
	ast.Extend(t.transformBody(body))

	return ast.ToPair()
}

func (t *LetTransformer) transformBindings(sexpr *types.Pair) *types.Pair {
	bindings := types.NewAppendablePair()
	head := sexpr
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

func (t *LetTransformer) transformBinding(binding *types.Pair) *types.Pair {
	if binding == nil || binding.IsNull() || !binding.HasNext() {
		return t.transformPair(binding)
	}

	switch sym := binding.This.(type) {
	case types.Symbol:
		// first transform binding, so not to shawdow it's name accidentally
		// as in (let ((x x)) ...)
		var val types.Sexpr
		if obj, ok := t.mappings[sym]; ok {
			val = obj
		} else {
			val = sym
		}

		name := newName(sym, t.suffix)
		t.mappings[sym] = name

		return types.NewPair(name, val)
	default:
		return t.transformPair(binding)
	}
}

func (t *LetTransformer) transformBody(sexpr *types.Pair) *types.Pair {
	ast := types.NewAppendablePair()
	head := sexpr
	for head != nil {
		var val types.Sexpr
		switch sym := head.This.(type) {
		case types.Symbol:
			val = t.transformSymbol(sym)
		default:
			val = t.transformSexpr(head.This)
		}
		ast.Append(val)
		head = head.Next
	}
	return ast.ToPair()
}

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
