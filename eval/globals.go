package eval

import (
	"github.com/twolodzko/gosch/envir"
)

var Debug = false

type (
	Procedure   = func(any, *envir.Env) (any, error)
	TailCallOpt = func(any, *envir.Env) (any, *envir.Env, error)
)

type Callable interface {
	Call(any, *envir.Env) (any, *envir.Env, error)
}
