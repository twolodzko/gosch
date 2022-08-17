// `define-syntax` and `syntax-rules` macros
//
// See:
//
// R. Kent Dybvig, Robert Hieb, and Carl Bruggeman (1993). "Syntactic abstraction in Scheme."
// Lisp and Symbolic Computation, 5(4):295-326. https://legacy.cs.indiana.edu/~dyb/pubs/LaSC-5-4-pp295-326.pdf
//
// R. Kent Dybvig (2009). "The Scheme Programming Language. Chapter 8. Syntactic Extension." The MIT Press.
// https://www.scheme.com/tspl4/syntax.html#./syntax:h0
//
// T. Shido "Yet Another Scheme Tutorial. 15. Defining Syntax" http://www.shido.info/lisp/scheme_syntax_e.html
//
// Paul Wilson (1997). "An Introduction to Scheme and its Implementation. Defining New Special Forms."
// https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v14/schintro_130.html
//
// Matthieu Felix (13 May 2020). "Macro systems in Scheme." https://terbium.io/2020/05/macros-scheme/
//
// "Guide: PLT Scheme. 16.1 Pattern-Based Macros." https://cs.brown.edu/courses/cs173/2008/Manual/guide/pattern-macros.html
//
// Alan Bawden and Jonathan Rees (1988). "Syntactic Closures."
// https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.18.3867

package macros

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `define-syntax` procedure
//
// (define-syntax key
//    (syntax-rules (literal ...)
//       (pattern template)
//       ...))
//
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
//    (syntax-rules (literal ...)
//       (pattern template)
//       ...))
//
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
		pattern := patternFromPair(first, literals)
		if err := validatePattern(pattern.Patterns); err != nil {
			return SyntaxRules{}, err
		}
		rules.Append(SyntaxRule{*pattern, second})
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
