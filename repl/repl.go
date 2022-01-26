package repl

import (
	"bufio"
	"io"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

type Repl struct {
	reader *bufio.Reader
	env    *envir.Env
}

func NewRepl(in io.Reader) *Repl {
	env := envir.NewEnv()
	return &Repl{bufio.NewReader(in), env}
}

func (repl *Repl) Repl() ([]types.Sexpr, error) {
	cmd, err := repl.read()
	if err != nil {
		return nil, err
	}
	objs, env, err := eval.EvalString(cmd, repl.env)
	repl.env = env
	return objs, err
}
