package special_forms

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var _ eval.Callable = (*Lambda)(nil)

type Lambda struct {
	args      any
	body      types.Pair
	parentEnv *envir.Env
}

// Create `lambda` function
//
//	(lambda (args ...) body ...)
func NewLambda(def any, env *envir.Env) (any, error) {
	args, tail, ok := unpack(def)
	if !ok {
		return nil, eval.ErrSyntax
	}
	body, ok := tail.(types.Pair)
	if !ok {
		return nil, eval.ErrSyntax
	}
	return Lambda{args, body, env}, nil
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
func (l Lambda) Call(args any, env *envir.Env) (any, *envir.Env, error) {
	local := envir.NewEnvFrom(l.parentEnv)
	if err := bindArgs(l.args, args, env, local); err != nil {
		return nil, nil, err
	}
	// the body of the function is evaluated in the local env of the lambda
	return eval.PartialEval(l.body, local)
}

func (l Lambda) String() string {
	body := l.body.ToString()
	return fmt.Sprintf("(lambda %v %v)", types.ToString(l.args), body)
}

func bindArgs(args, vals any, evalEnv, bindEnv *envir.Env) error {
	for {
		switch a := args.(type) {
		case types.Pair:
			name, ok := a.This.(types.Symbol)
			if !ok {
				return eval.InvalidName{Val: a.This}
			}
			v, ok := vals.(types.Pair)
			if !ok {
				return eval.ErrArity
			}
			val, err := eval.Eval(v.This, evalEnv)
			if err != nil {
				return err
			}
			bindEnv.Set(name, val)
			// next arg
			args = a.Next
			vals = v.Next
		case nil:
			// end of list or arg names
			if vals != nil {
				return eval.ErrArity
			}
			return nil
		case types.Symbol:
			// it was a dotted pair
			var (
				val any
				err error
			)
			switch v := vals.(type) {
			case nil:
			case types.Pair:
				var l []any
				l, err = v.TryMap(func(val any) (any, error) {
					return eval.Eval(val, evalEnv)
				})
				val = types.Cons(l...)
			default:
				val, err = eval.Eval(vals, evalEnv)
			}
			bindEnv.Set(a, val)
			return err
		default:
			return eval.InvalidName{Val: a}
		}
	}
}
