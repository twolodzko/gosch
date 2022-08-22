package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/macros/pattern"
	"github.com/twolodzko/gosch/macros/template"
	"github.com/twolodzko/gosch/types"
)

// `define-syntax` procedure
//
// (define-syntax key
//
//	(syntax-rules (literal ...)
//	   (pattern template)
//	   ...))
func DefineSyntax(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() || args.Next.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	key, ok := args.This.(types.Symbol)
	if !ok {
		return nil, eval.NewErrBadName(args.This)
	}

	obj, err := eval.Eval(args.Next.This, env)
	if err != nil {
		return nil, err
	}
	rules, ok := obj.(SyntaxRules)
	if !ok {
		return nil, fmt.Errorf("expecting syntax rules, got %s", rules)
	}

	env.Set(key, rules)
	return rules, nil
}

// `syntax-rules` procedure
//
//	(syntax-rules (literal ...)
//	   (pattern template)
//	   ...))
func NewSyntaxRules(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return SyntaxRules{}, eval.ErrBadArgNumber
	}
	literals, err := extractLiterals(args.This)
	if err != nil {
		return SyntaxRules{}, err
	}
	return extractSyntaxRules(args.Next, literals)
}

func extractLiterals(arg types.Sexpr) ([]types.Symbol, error) {
	pair, ok := arg.(*types.Pair)
	if !ok {
		return nil, eval.NewErrNonList(arg)
	}
	if pair == nil || pair.IsNull() {
		return nil, nil
	}
	var literals []types.Symbol
	head := pair
	for head != nil {
		name, ok := head.This.(types.Symbol)
		if !ok {
			return nil, eval.NewErrBadName(head.This)
		}
		literals = append(literals, name)
		head = head.Next
	}
	return literals, nil
}

func extractSyntaxRules(args *types.Pair, literals []types.Symbol) (SyntaxRules, error) {
	rules := SyntaxRules{literals, nil}
	head := args
	for head != nil {
		first, second, err := maybeSyntaxRule(head)
		if err != nil {
			return SyntaxRules{}, err
		}

		pattern, err := pattern.FromPair(first, literals)
		if err != nil {
			return SyntaxRules{}, err
		}

		template, err := template.Parse(second)
		if err != nil {
			return SyntaxRules{}, err
		}

		rules.Append(SyntaxRule{*pattern, template})
		head = head.Next
	}
	return rules, nil
}

func maybeSyntaxRule(obj *types.Pair) (*types.Pair, types.Sexpr, error) {
	pair, ok := obj.This.(*types.Pair)
	if !ok {
		return nil, nil, eval.NewErrNonList(obj.This)
	}

	first, ok := pair.This.(*types.Pair)
	if !ok {
		return nil, nil, eval.NewErrNonList(pair.This)
	}
	if first == nil || first.IsNull() {
		return nil, nil, fmt.Errorf("empty pattern")
	}

	// ignore name of the procedure at the beginning of the pattern
	first = first.Next

	return first, pair.Next.This, nil
}
