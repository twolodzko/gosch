package envir

import (
	"github.com/twolodzko/gosch/types"
)

type Env struct {
	Vars   map[types.Symbol]types.Sexpr
	Parent *Env
}

func NewEnv() *Env {
	vars := make(map[types.Symbol]types.Sexpr)
	return &Env{vars, nil}
}

func NewEnvFrom(parent *Env) *Env {
	new := NewEnv()
	new.Parent = parent
	return new
}

func (e *Env) Set(name types.Symbol, value types.Sexpr) {
	e.Vars[name] = value
}

// Find an enclosing environment for the variable
func (e *Env) FindEnv(name types.Symbol) (*Env, bool) {
	current := e
	for current != nil {
		if _, ok := current.Vars[name]; ok {
			return current, true
		}
		current = current.Parent
	}
	return nil, false
}

func (e *Env) Get(name types.Symbol) (types.Sexpr, bool) {
	if env, ok := e.FindEnv(name); ok {
		return env.Vars[name], true
	}
	return nil, false
}
