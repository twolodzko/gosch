package scheme

import (
	"reflect"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func sameCallables(obj1, obj2 types.Sexpr) bool {
	v1 := reflect.ValueOf(obj1)
	v2 := reflect.ValueOf(obj2)
	if v1.Kind() == reflect.Func {
		if v2.Kind() == reflect.Func {
			return v1.Pointer() == v2.Pointer()
		}
	}
	if v1, ok := obj1.(eval.Lambda); ok {
		if v2, ok := obj2.(eval.Lambda); ok {
			return cmp.Equal(v1, v2)
		}
	}
	return false
}

func Eq(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	// you can't compare functions and structs directly in Go
	if isCallable(args.This) {
		return types.Bool(sameCallables(args.This, args.Next.This)), nil
	}
	return types.Bool(args.This == args.Next.This), nil
}

func Equal(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Equal(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}

func Lower(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Lower(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}

func Greater(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Greater(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}
