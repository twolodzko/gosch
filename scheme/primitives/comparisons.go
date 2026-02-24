package primitives

import (
	"reflect"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func Eq(args any, env *envir.Env) (any, error) {
	lhs, rhs, err := eval.EvalTwo(args, env)
	if err != nil {
		return nil, err
	}
	// TODO: validate it again
	// you can't compare functions and structs directly in Go
	if isCallable(lhs) {
		return sameCallables(lhs, rhs), nil
	}
	return reflect.DeepEqual(lhs, rhs), nil
}

func Equal(args any, env *envir.Env) (any, error) {
	if args == nil {
		return true, nil
	}
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	var last = vals[0]
	for i := 1; i < len(vals); i++ {
		if last != vals[i] {
			return false, nil
		}
		last = vals[i]
	}
	return true, nil
}

func Lower(args any, env *envir.Env) (any, error) {
	if args == nil {
		return nil, eval.ArityError
	}
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	last, ok := vals[0].(types.Comparable)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	for i := 1; i < len(vals); i++ {
		if ok, err := last.Lower(vals[i]); !ok || err != nil {
			return false, err
		}
		last = vals[i].(types.Comparable)
	}
	return true, nil
}

func Greater(args any, env *envir.Env) (any, error) {
	if args == nil {
		return nil, eval.ArityError
	}
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	last, ok := vals[0].(types.Comparable)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	for i := 1; i < len(vals); i++ {
		if ok, err := last.Greater(vals[i]); !ok || err != nil {
			return false, err
		}
		last = vals[i].(types.Comparable)
	}
	return true, nil
}

func sameCallables(obj1, obj2 any) bool {
	v1 := reflect.ValueOf(obj1)
	v2 := reflect.ValueOf(obj2)
	if v1.Kind() == reflect.Func {
		if v2.Kind() == reflect.Func {
			return v1.Pointer() == v2.Pointer()
		}
	}
	if v1, ok := obj1.(eval.Callable); ok {
		if v2, ok := obj2.(eval.Callable); ok {
			return reflect.DeepEqual(v1, v2)
		}
	}
	return false
}
