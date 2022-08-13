package macros

import (
	"fmt"
	"strings"

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

func (m SyntaxRules) Apply(obj types.Sexpr) (types.Sexpr, bool) {
	for _, macro := range m.rules {
		if mapping, ok := macro.pattern.Match(obj); ok {
			t := newTransformer(mapping)
			sexpr := t.transform(macro.template)
			return sexpr, true
		}
	}
	return nil, false
}

func (m SyntaxRules) String() string {
	literals := strings.Join(m.literals, " ")
	var rules []string
	for _, rule := range m.rules {
		rules = append(rules, fmt.Sprintf("(%s %s)", rule.pattern, rule.template))
	}
	return fmt.Sprintf("(syntax-rules (%s) %s)", literals, strings.Join(rules, " "))
}
