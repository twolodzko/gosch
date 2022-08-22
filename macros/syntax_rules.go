package macros

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/macros/pattern"
	"github.com/twolodzko/gosch/macros/template"
	"github.com/twolodzko/gosch/types"
)

type SyntaxRule struct {
	pattern  pattern.Pair
	template types.Sexpr
}

type SyntaxRules struct {
	literals []types.Symbol
	rules    []SyntaxRule
}

func (m *SyntaxRules) Append(rule SyntaxRule) {
	m.rules = append(m.rules, rule)
}

func (m SyntaxRules) Transform(obj types.Sexpr, env *envir.Env) (types.Sexpr, error) {
	for _, macro := range m.rules {
		if mapping, ok := macro.pattern.Match(obj); ok {
			return template.Transform(macro.template, mapping)
		}
	}
	return nil, fmt.Errorf("%s didn't match any pattern", obj)
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
	sexpr, err := m.Transform(args, env)
	return sexpr, env, err
}
