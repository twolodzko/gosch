package primitives

import (
	"fmt"
	"time"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func Display(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		fmt.Println()
		return nil, nil
	}
	fmt.Printf("%v\n", args.ElemsToString())
	return nil, nil
}

// `error` procedure
func Error(args *types.Pair) (types.Sexpr, error) {
	return nil, fmt.Errorf("%v", args.ElemsToString())
}

// `debug` procedure
func Debug(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		eval.DEBUG = true
		return types.Bool(eval.DEBUG), nil
	}
	if args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	eval.DEBUG = bool(types.IsTrue(args.This))
	return types.Bool(eval.DEBUG), nil
}

// `timeit` procedure
func Timeit(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	start := time.Now()
	result, err := eval.EvalAll(args, env)
	if err != nil {
		return nil, err
	}
	fmt.Println(time.Since(start))
	return result, nil
}
