package repl

import (
	"bufio"
	"io"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

type Any = types.Any

type Repl struct {
	reader *bufio.Reader
	env    *envir.Env
}

func NewRepl(in io.Reader) *Repl {
	env := envir.NewEnv()
	return &Repl{bufio.NewReader(in), env}
}

func (repl *Repl) Repl() ([]Any, error) {
	cmd, err := repl.read()
	if err != nil {
		return nil, err
	}
	objs, env, err := eval.EvalString(cmd, repl.env)
	repl.env = env
	return objs, err
}
