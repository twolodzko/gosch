package eval

import (
	"fmt"
	"reflect"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func car(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.This, nil
	default:
		return nil, &ErrNonList{args.This}
	}
}

func cdr(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
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
		return nil, &ErrNonList{args.This}
	}
}

func cons(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, ErrBadArgNumber
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
		return nil, ErrBadArgNumber
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

// `null?` procedure
func isNull(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

// `pair?` procedure
func isPair(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch val := args.This.(type) {
	case *types.Pair:
		return !val.IsNull(), nil
	default:
		return types.Bool(false), nil
	}
}

// `bool?` procedure
func isBool(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Bool:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

// `string?` procedure
func isString(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.String:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

// `symbol?` procedure
func isSymbol(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.Symbol:
		return types.Bool(true), nil
	default:
		return types.Bool(false), nil
	}
}

// `nil?` procedure
func isNil(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	return types.Bool(args.This == nil), nil
}

// `procedure?` procedure
func isProcedure(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, ErrBadArgNumber
	}
	return types.Bool(isCallable(args.This)), nil
}

// Convert arguments to string, using space as a separator
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

func asInt(s types.Sexpr) (int, error) {
	switch x := s.(type) {
	case types.Integer:
		return int(x), nil
	default:
		return 0, fmt.Errorf("%v is not an integer", s)
	}
}

func substring(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return nil, ErrBadArgNumber
	}
	str, ok := args.This.(types.String)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", args.This)
	}
	start, err := asInt(args.Next.This)
	if err != nil {
		return nil, err
	}
	end, err := asInt(args.Next.Next.This)
	if err != nil {
		return nil, err
	}
	if start < 0 || end < start || end > len(str) {
		return nil, fmt.Errorf("cannot produce substring %v:%v", start, end)
	}
	return str[start:end], nil
}

// `string-length` procedure
func stringLength(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, ErrBadArgNumber
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

// `error?` procedure
func raiseError(args *types.Pair) (types.Sexpr, error) {
	return nil, fmt.Errorf("%s", toString(args, " "))
}

func debug(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		DEBUG = true
		return types.Bool(DEBUG), nil
	}
	if args.HasNext() {
		return nil, ErrBadArgNumber
	}
	DEBUG = bool(types.IsTrue(args.This))
	return types.Bool(DEBUG), nil
}
