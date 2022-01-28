package eval

import (
	"errors"
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type Lambda struct {
	Vars      []types.Symbol
	Body      *types.Pair
	ParentEnv *envir.Env
}

func newLambda(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch pair := args.This.(type) {
	case *types.Pair:
		vars, err := lambdaArgs(pair)
		return Lambda{vars, args.Next, env}, err
	default:
		return Lambda{}, fmt.Errorf("%v is not a list", args.This)
	}
}

func lambdaArgs(args *types.Pair) ([]types.Symbol, error) {
	var vars []types.Symbol
	if args == nil || args.IsNull() {
		return vars, nil
	}
	head := args
	for head != nil {
		if name, ok := head.This.(types.Symbol); ok {
			vars = append(vars, name)
		} else {
			return vars, fmt.Errorf("%v is not a valid argument name", args.This)
		}
		head = head.Next
	}
	return vars, nil
}

func (l Lambda) Call(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {
	// Example:
	//
	// (define x 4)    ;; parent env
	// (define addX
	//   (lambda (n)   ;; n would be defined at function call
	// 	   (+ x n)))   ;; this x comes from parent env
	// (let ((x 3))    ;; setting up local env
	//   (addX
	//     (+ x 7)))   ;; this x comes from local env
	// => (+ 4 (+ 3 7)) = 14
	//      /     |  \
	//  parent  local  calling env

	// local env inherits from the env where the lambda was defined
	local := envir.NewEnv()
	local.Parent = l.ParentEnv

	// setup local env
	head := args
	for _, name := range l.Vars {
		if head == nil {
			return nil, local, errors.New("wrong number of arguments")
		}
		// arguments are evaluated in the env enclosing the lambda call
		val, err := Eval(head.This, env)
		if err != nil {
			return nil, local, err
		}
		local.Set(name, val)
		head = head.Next
	}

	// the body of the function is evaluated in the local env of the lambda
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
