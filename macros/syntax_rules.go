package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

type SyntaxRule struct {
	pattern  PairPattern
	template types.Sexpr
}

type SyntaxRules struct {
	literals []types.Symbol
	rules    []SyntaxRule
}

func (m *SyntaxRules) Append(rule SyntaxRule) {
	m.rules = append(m.rules, rule)
}

func (m SyntaxRules) Apply(obj types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	for _, macro := range m.rules {
		if mapping, ok := macro.pattern.Match(obj); ok {
			t := newTransformer(mapping, env)
			return t.transform(macro.template)
		}
	}
	return nil, fmt.Errorf("the arguments didn't match any pattern")
}

func (m SyntaxRules) String() string {
	literals := strings.Join(m.literals, " ")
	var rules []string
	for _, rule := range m.rules {
		rules = append(rules, fmt.Sprintf("(%s %s)", rule.pattern, rule.template))
	}
	return fmt.Sprintf("(syntax-rules (%s) %s)", literals, strings.Join(rules, " "))
}

// SyntaxRules follows the eval.Callable interface
func (m SyntaxRules) Call(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	sexpr, err := m.Apply(args, env)
	return sexpr, env, err
}

// `expand-macro` procedure
func ExpandMacro(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	var (
		obj types.Sexpr = args.This
		ok  bool
	)
	for {
		switch m := obj.(type) {
		case types.Symbol:
			obj, ok = env.Get(m)
			if !ok {
				return nil, fmt.Errorf("%s is not a macro", obj)
			}
		case SyntaxRules:
			sexpr, _, err := m.Call(args.Next, env)
			return sexpr, err
		default:
			return nil, fmt.Errorf("%s is not a macro", obj)
		}
	}
}
