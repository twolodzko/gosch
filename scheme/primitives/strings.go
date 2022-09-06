package primitives

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `string` procedure
func ToString(args *types.Pair) (types.Sexpr, error) {
	return types.String(joinToString(args, "")), nil
}

// `string?` procedure
func IsString(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case types.String:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

func asInt(s types.Sexpr) (int, error) {
	switch x := s.(type) {
	case types.Integer:
		return int(x), nil
	default:
		return 0, fmt.Errorf("%v is not an integer", s)
	}
}

func Substring(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() || !args.Next.HasNext() {
		return nil, eval.ErrBadArgNumber
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
func StringLength(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	str, ok := args.This.(types.String)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", args.This)
	}
	return len(str), nil
}

// Convert arguments to string, using space as a separator
func joinToString(args *types.Pair, sep string) string {
	var s []string
	head := args
	for head != nil {
		switch val := head.This.(type) {
		case types.String:
			s = append(s, fmt.Sprintf("%v", string(val)))
		default:
			s = append(s, fmt.Sprintf("%v", val))
		}
		head = head.Next
	}
	return strings.Join(s, sep)
}
