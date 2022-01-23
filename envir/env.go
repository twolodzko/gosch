package envir

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

type Env struct {
	Vars   map[types.Symbol]types.Any
	Parent *Env
}

func NewEnv() *Env {
	vars := make(map[types.Symbol]types.Any)
	return &Env{vars, nil}
}

func (e *Env) Set(name types.Symbol, value types.Any) {
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

func (e *Env) Get(name types.Symbol) (types.Any, error) {
	if env, ok := e.FindEnv(name); ok {
		return env.Vars[name], nil
	}
	return nil, fmt.Errorf("unbound variable %v", name)
}
