package eval

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

var DEBUG bool = false

type (
	Primitive         = func(*types.Pair) (types.Sexpr, error)
	Procedure         = func(*types.Pair, *envir.Env) (types.Sexpr, error)
	TailCallOptimized = func(*types.Pair, *envir.Env) (types.Sexpr, *envir.Env, error)
)

type Callable interface {
	Call(*types.Pair, *envir.Env) (types.Sexpr, *envir.Env, error)
}

// Available procedures to be imported from other modules
type ProceduresGetter = map[types.Symbol]interface{}

var Procedures ProceduresGetter
