package repl

import (
	"bufio"
	"io"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
)

type Repl struct {
	reader *bufio.Reader
	env    *envir.Env
}

func NewRepl(in io.Reader, env *envir.Env) *Repl {
	return &Repl{bufio.NewReader(in), env}
}

func (repl *Repl) Repl() ([]any, error) {
	cmd, err := repl.read()
	if err != nil {
		return nil, err
	}
	objs, env, err := eval.EvalString(cmd, repl.env)
	repl.env = env
	return objs, err
}
