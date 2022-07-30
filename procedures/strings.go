package procedures

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `string` procedure
func toString(args *types.Pair) (types.Sexpr, error) {
	return types.String(toRawString(args, "")), nil
}

// Convert arguments to string, using space as a separator
func toRawString(args *types.Pair, sep string) string {
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
func stringLength(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	str, ok := args.This.(types.String)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", args.This)
	}
	return len(str), nil
}
