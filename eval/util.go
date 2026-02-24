package eval

import (
	"os"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/parser"
)

func EvalString(code string, env *envir.Env) ([]any, *envir.Env, error) {
	var out []any
	parser := parser.NewParser(code)
	sexprs, err := parser.Read()
	if err != nil {
		return nil, env, err
	}
	for _, sexpr := range sexprs {
		result, err := Eval(sexpr, env)
		if err != nil {
			return nil, env, err
		}
		out = append(out, result)
	}
	return out, env, err
}

func LoadEval(path string, env *envir.Env) ([]any, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	sexprs, _, err := EvalString(string(content), env)
	return sexprs, err
}
