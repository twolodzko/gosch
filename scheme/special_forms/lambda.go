package special_forms

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.Callable = (*Lambda)(nil)

type Lambda struct {
	Vars      []types.Symbol
	Body      *types.Pair
	ParentEnv *envir.Env
}

// Create `lambda` function
//
//	(lambda (args ...) body ...)
func NewLambda(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return Lambda{}, eval.ErrBadArgNumber
	}
	switch pair := args.This.(type) {
	case *types.Pair:
		vars, err := extractSymbols(pair)
		return Lambda{vars, args.Next, env}, err
	default:
		return Lambda{}, eval.NewErrNonList(args.This)
	}
}

// Transform pair to slice
func extractSymbols(args *types.Pair) ([]types.Symbol, error) {
	var vars []types.Symbol
	if args == nil || args.IsNull() {
		return vars, nil
	}
	head := args
	for head != nil {
		if name, ok := head.This.(types.Symbol); ok {
			vars = append(vars, name)
		} else {
			return vars, eval.NewErrBadName(args.This)
		}
		head = head.Next
	}
	return vars, nil
}

// Call `lambda` function
//
//	 Example:
//
//	 (define x 4)    ;; parent env
//	 (define addX
//	   (lambda (n)   ;; n would be defined at function call
//		    (+ x n)))   ;; this x comes from parent env
//	 (let ((x 3))    ;; setting up local env
//	   (addX
//	     (+ x 7)))   ;; this x comes from local env
//	 => (+ 4 (+ 3 7)) = 14
//	      /     |  \
//	  parent  local  calling env
func (l Lambda) Call(args *types.Pair, env *envir.Env) (types.Sexpr, *envir.Env, error) {

	local, err := l.createClosure(args, env)
	if err != nil {
		return nil, local, err
	}

	// the body of the function is evaluated in the local env of the lambda
	return eval.PartialEval(l.Body, local)
}

func (l Lambda) createClosure(args *types.Pair, env *envir.Env) (*envir.Env, error) {
	// local env inherits from the env where the lambda was defined
	local := envir.NewEnvFrom(l.ParentEnv)

	// setup local env
	head := args
	for _, name := range l.Vars {
		if head == nil {
			return local, eval.ErrBadArgNumber
		}
		// arguments are evaluated in the env enclosing the lambda call
		val, err := eval.Eval(head.This, env)
		if err != nil {
			return local, err
		}
		local.Set(name, val)
		head = head.Next
	}

	if head != nil && head.HasNext() {
		return local, eval.ErrBadArgNumber
	}

	return local, nil
}

func (l Lambda) String() string {
	vars := strings.Join(l.Vars, " ")
	body := l.Body.ToString()
	return fmt.Sprintf("(lambda (%v) %v)", vars, body)
}
