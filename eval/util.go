package eval

import (
	"os"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func EvalString(code string, env *envir.Env) ([]types.Sexpr, *envir.Env, error) {
	var out []types.Sexpr
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

func LoadEval(path string, env *envir.Env) ([]types.Sexpr, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	sexprs, _, err := EvalString(string(content), env)
	return sexprs, err
}
