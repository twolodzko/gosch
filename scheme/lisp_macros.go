package scheme

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/macros"
	"github.com/twolodzko/gosch/types"
)

var _ macros.Macro = (*LispMacro)(nil)

type LispMacro struct {
	Vars     []types.Symbol
	Template types.Sexpr
}

func (m LispMacro) Transform(sexpr types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	return m.Template, nil
}

func (m LispMacro) Call(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	local, err := evalAndAssign(args, env, env, m.Vars)
	if err != nil {
		return nil, local, err
	}

	sexpr, err := eval.Eval(m.Template, local)
	return sexpr, env, err
}

func (m LispMacro) String() string {
	vars := strings.Join(m.Vars, " ")
	template := ""
	switch obj := m.Template.(type) {
	case *types.Pair:
		head := obj
		for head != nil {
			template += fmt.Sprintf("%v", head.This)
			if head.HasNext() {
				template += " "
			}
			head = head.Next
		}
	default:
		template += fmt.Sprintf("%v", obj)
	}
	return fmt.Sprintf("(macro (%v) (%v))", vars, template)
}

// Create `macro` function
//
//	(macro (args ...) body ...)
func NewMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return LispMacro{}, eval.ErrBadArgNumber
	}
	switch pair := args.This.(type) {
	case *types.Pair:
		vars, err := SymbolsPairToSlice(pair)
		return LispMacro{vars, args.Next.This}, err
	default:
		return LispMacro{}, eval.NewErrNonList(args.This)
	}
}

// `define-macro` procedure
func DefineMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	switch this := args.This.(type) {
	case *types.Pair:
		return defineMacro(this, args.Next.This, env)
	default:
		return nil, eval.NewErrBadName(args.This)
	}
}

func defineMacro(args *types.Pair, template types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	name, ok := args.This.(types.Symbol)
	if !ok {
		return nil, eval.NewErrBadName(args.This)
	}
	vars, err := SymbolsPairToSlice(args.Next)
	if err != nil {
		return nil, err
	}
	fn := LispMacro{vars, template}
	env.Set(name, fn)
	return fn, nil
}
