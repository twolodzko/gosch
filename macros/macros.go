package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

const (
	Self     = types.Symbol("_")
	Ellipsis = types.Symbol("...")
)

//  (syntax-rules (literal ...) (pattern template) ...)

// type Transformer struct {
// 	// applies the pattern
// }

func ExtractPattern(pair *types.Pair, self types.Symbol, literals []types.Symbol) (PairPattern, error) {
	var (
		pattern  Pattern
		patterns []Pattern
		err      error
	)

	if pair.IsNull() {
		return PairPattern{}, nil
	}

	head := pair
	for head != nil {
		switch obj := head.This.(type) {
		case types.Symbol:
			switch {
			case isLiteral(obj, literals):
				pattern = LiteralPattern{obj}
			case obj == Self:
				pattern = SelfPattern{self}
			case obj == Ellipsis:
				if head.HasNext() {
					return PairPattern{}, fmt.Errorf("... needs to appear at the end of the pattern")
				}
				pattern = EllipsisPattern{}
			default:
				pattern = IdentifierPattern{obj}
			}
		case *types.Pair:
			pattern, err = ExtractPattern(obj, self, literals)
			if err != nil {
				return PairPattern{}, err
			}
		default:
			pattern = LiteralPattern{obj}
		}
		patterns = append(patterns, pattern)
		head = head.Next
	}

	return PairPattern{patterns}, nil
}

func isLiteral(obj types.Symbol, literals []types.Symbol) bool {
	for _, l := range literals {
		if obj == l {
			return true
		}
	}
	return false
}

// `define-syntax` procedure
func DefineSyntax(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() || args.Next.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	// name, ok := args.This.(types.Symbol)
	// if !ok {
	// 	return nil, eval.NewErrBadName(args.This)
	// }
	// syntax, ok := args.Next.This.(*types.Pair)
	// if !ok {
	// 	return nil, eval.NewErrNonList(args.Next.This)
	// }
	// if syntax == nil {
	// 	return nil, fmt.Errorf("expecting syntax rules, got %s", syntax)
	// }
	// if op, ok := syntax.This.(types.Symbol); !ok || op != "syntax-rules" {
	// 	return nil, fmt.Errorf("expecting syntax rules, got %s", syntax)
	// }

	return nil, nil
}

// `syntax-rules` procedure
func SyntaxRules(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	return nil, fmt.Errorf("syntax-rules cannot be used outside of define-syntax")
}
