package eval

import (
	"errors"
	"fmt"
	"reflect"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func car(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.This, nil
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func cdr(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.This.(type) {
	case *types.Pair:
		if val.IsNull() {
			return nil, nil
		}
		switch {
		case val.Next == nil:
			return &types.Pair{}, nil
		default:
			return val.Next, nil
		}
	default:
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
}

func cons(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.Next.This.(type) {
	case *types.Pair:
		return val.Cons(args.This), nil
	default:
		return list(args)
	}
}

func list(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return &types.Pair{}, nil
	}
	return args, nil
}

func not(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return types.Bool(false), nil
	}
	return !types.IsTrue(args.This), nil
}

func eq(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	// you can't compare functions and structs directly in Go
	if isCallable(args.This) {
		return types.Bool(sameCallables(args.This, args.Next.This)), nil
	}
	return types.Bool(args.This == args.Next.This), nil
}

func sameCallables(obj1, obj2 types.Sexpr) bool {
	v1 := reflect.ValueOf(obj1)
	v2 := reflect.ValueOf(obj2)
	if v1.Kind() == reflect.Func {
		if v2.Kind() == reflect.Func {
			return v1.Pointer() == v2.Pointer()
		}
	}
	if v1, ok := obj1.(Lambda); ok {
		if v2, ok := obj2.(Lambda); ok {
			return cmp.Equal(v1, v2)
		}
	}
	return false
}

func isNull(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

func isPair(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return !val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

func isNumber(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case int:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isBool(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.Bool:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isString(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.String:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isSymbol(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	switch args.This.(type) {
	case types.Symbol:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

func isNil(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	return types.Bool(args.This == nil), nil
}

func isProcedure(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	return types.Bool(isCallable(args.This)), nil
}

func toString(args *types.Pair, sep string) string {
	if args == nil {
		return ""
	}
	var out string
	head := args
	for head != nil {
		switch val := head.This.(type) {
		case types.String:
			out += fmt.Sprintf("%v", string(val))
		default:
			out += fmt.Sprintf("%v", val)
		}

		if head.HasNext() {
			out += sep
		}
		head = head.Next
	}
	return out
}

func substring(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	str, ok := args.This.(types.String)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", args.This)
	}
	start, err := toInt(args.Next.This)
	if err != nil {
		return nil, err
	}
	end, err := toInt(args.Next.Next.This)
	if err != nil {
		return nil, err
	}
	if start < 0 || end < start || end > len(str) {
		return nil, fmt.Errorf("cannot produce substring %v:%v", start, end)
	}
	return str[start:end], nil
}

func stringLength(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	str, ok := args.This.(types.String)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", args.This)
	}
	return len(str), nil
}

func display(args *types.Pair) (types.Sexpr, error) {
	fmt.Printf("%s\n", toString(args, " "))
	return nil, nil
}

func raiseError(args *types.Pair) (types.Sexpr, error) {
	return nil, fmt.Errorf("%s", toString(args, " "))
}

func debug(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		DEBUG = true
		return types.Bool(DEBUG), nil
	}
	if args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	DEBUG = bool(types.IsTrue(args.This))
	return types.Bool(DEBUG), nil
}
