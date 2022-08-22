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
	"github.com/twolodzko/gosch/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

type Macro interface {
	Transform(types.Sexpr, *envir.Env) (types.Sexpr, error)
}

// `expand-macro` procedure
func ExpandMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	var (
		obj types.Sexpr = args.This
		ok  bool
		err error
	)
	for {
		switch m := obj.(type) {
		case types.Symbol:
			obj, ok = env.Get(m)
			if !ok {
				return nil, fmt.Errorf("%s is not a macro", obj)
			}
		case *types.Pair:
			obj, err = eval.Eval(m, env)
			if err != nil {
				return nil, err
			}
		case Macro:
			sexpr, err := m.Transform(args.Next, env)
			return sexpr, err
		default:
			return nil, fmt.Errorf("%s is not a macro", obj)
		}
	}
}

// `gensym` procedure
func Gensym(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args != nil {
		return nil, eval.ErrBadArgNumber
	}
	return types.Symbol(gensym.Generator.New()), nil
}
