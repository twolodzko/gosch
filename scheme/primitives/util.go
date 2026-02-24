package primitives

import (
	"fmt"
	"time"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `display` procedure
func Display(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	fmt.Printf("%v\n", joinToString(vals, " "))
	return nil, err
}

// `newline` procedure
func Newline(args []any) (any, error) {
	if len(args) != 0 {
		return nil, eval.ErrArity
	}
	fmt.Println()
	return nil, nil
}

// `error` procedure
func Error(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	return nil, fmt.Errorf("%v", joinToString(vals, " "))
}

// `debug` procedure
func Debug(args any, env *envir.Env) (any, error) {
	if args == nil {
		eval.Debug = true
		return eval.Debug, nil
	}
	v, err := eval.EvalOne(args, env)
	eval.Debug = types.IsTrue(v)
	return eval.Debug, err
}

// `timeit` procedure
func Timeit(args any, env *envir.Env) (any, error) {
	start := time.Now()
	result, err := eval.EvalOne(args, env)
	fmt.Println(time.Since(start))
	return result, err
}
