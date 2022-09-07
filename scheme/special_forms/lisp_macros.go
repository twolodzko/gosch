// See:
//
// Alan Bawden and Jonathan Rees (1988). "Syntactic Closures."
// https://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.18.3867
//
// Paul Wilson (1997). "An Introduction to Scheme and its Implementation: Defining New Special Forms."
// https://www.cs.utexas.edu/ftp/garbage/cs345/schintro-v14/schintro_130.html#SEC183
//
// Matthieu Felix (13 May 2020). "Macro systems in Scheme." https://terbium.io/2020/05/macros-scheme/
//
// "Macros." https://docs.scheme.org/guide/macros/
//
// "syntactic-closures." http://community.schemewiki.org/?syntactic-closures

package special_forms

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/macros"
	"github.com/twolodzko/gosch/types"
)

var _ macros.Macro = (*LispMacro)(nil)
var _ eval.Callable = (*LispMacro)(nil)

type LispMacro struct {
	Vars     []types.Symbol
	Template *types.Pair
}

func (m LispMacro) Transform(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	local, err := makeSyntacticClosure(args, env, m.Vars)
	if err != nil {
		return nil, local, err
	}
	sexpr, err := traverseToUnquote(m.Template, 0, local)
	return sexpr, local, err
}

func (m LispMacro) Call(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	sexpr, local, err := m.Transform(args, env)
	if err != nil {
		return nil, local, err
	}
	sexpr, err = eval.Eval(sexpr, local)
	return sexpr, env, err
}

func (l LispMacro) String() string {
	vars := strings.Join(l.Vars, " ")
	template := l.Template.ToString()
	return fmt.Sprintf("(macro (%v) %v)", vars, template)
}

// Create `macro` function
//
//	(macro (args ...) template)
func NewLispMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return LispMacro{}, eval.ErrBadArgNumber
	}
	switch pair := args.This.(type) {
	case *types.Pair:
		vars, err := symbolsPairToSlice(pair)
		if err != nil {
			return LispMacro{}, err
		}
		template, ok := args.Next.This.(*types.Pair)
		if !ok {
			return LispMacro{}, eval.NewErrNonList(args.Next.This)
		}
		return LispMacro{vars, template}, err
	default:
		return LispMacro{}, eval.NewErrNonList(args.This)
	}
}

// `define-macro` procedure
//
// (define-macro (name args ...) template)
func DefineMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	first, ok := args.This.(*types.Pair)
	if !ok {
		return nil, eval.NewErrBadName(args.This)
	}
	name, ok := first.This.(types.Symbol)
	if !ok {
		return "", eval.NewErrBadName(args.This)
	}
	vars, err := symbolsPairToSlice(first.Next)
	if err != nil {
		return LispMacro{}, err
	}

	template, ok := args.Next.This.(*types.Pair)
	if !ok {
		return LispMacro{}, eval.NewErrNonList(args.Next.This)
	}

	macro := LispMacro{vars, template}
	env.Set(name, macro)
	return macro, nil
}

func makeSyntacticClosure(args *types.Pair, env *envir.Env, names []types.Symbol) (*envir.Env, error) {
	local := envir.NewEnvFrom(env)

	head := args
	for _, name := range names {
		if head == nil {
			return local, eval.ErrBadArgNumber
		}
		local.Set(name, head.This)
		head = head.Next
	}

	if head != nil && head.HasNext() {
		return local, eval.ErrBadArgNumber
	}

	return local, nil
}
