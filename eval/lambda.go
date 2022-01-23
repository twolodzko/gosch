package eval

import (
	"errors"
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type Lambda struct {
	Vars []types.Symbol
	Body *types.Pair
}

func newLambda(args *types.Pair, env *envir.Env) (types.Any, error) {
	if args.IsNull() || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}

	var vars []types.Symbol
	switch pair := args.This.(type) {
	case *types.Pair:
		if pair.IsNull() {
			return Lambda{nil, args.Next}, nil
		}

		head := pair
		for head != nil {
			if name, ok := head.This.(types.Symbol); ok {
				vars = append(vars, name)
			} else {
				return Lambda{}, fmt.Errorf("%v is not a valid argument name", pair.This)
			}
			head = head.Next
		}
	default:
		return Lambda{}, fmt.Errorf("%v is not a list", args.This)
	}
	return Lambda{vars, args.Next}, nil
}

func (l Lambda) Call(args *types.Pair, env *envir.Env) (types.Any, *envir.Env, error) {
	local := envir.NewEnv()
	local.Parent = env

	head := args
	for _, name := range l.Vars {
		if head == nil {
			return nil, local, errors.New("wrong number of arguments")
		}
		// arguments are evaluated in the env enclosing the lambda
		val, err := Eval(head.This, env)
		if err != nil {
			return nil, local, err
		}
		local.Set(name, val)
		head = head.Next
	}
	return partialEval(l.Body, local)
}

func (l Lambda) String() string {
	vars := strings.Join(l.Vars, " ")
	body := ""
	head := l.Body
	for head != nil {
		body += fmt.Sprintf("%v", head.This)
		if head.HasNext() {
			body += " "
		}
		head = head.Next
	}
	return fmt.Sprintf("(lambda (%v) %v)", vars, body)
}
