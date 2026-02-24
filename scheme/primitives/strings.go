package primitives

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `string` procedure
func ToString(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	return joinToString(vals, ""), err
}

// `string?` procedure
func IsString(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	_, ok := v.(string)
	return ok, err
}

func asInt(s any) (int, error) {
	switch x := s.(type) {
	case types.Integer:
		return int(x), nil
	default:
		return 0, fmt.Errorf("%v is not an integer", s)
	}
}

func Substring(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) != 3 {
		return nil, eval.ErrArity
	}
	str, ok := vals[0].(string)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", vals[0])
	}
	start, err := asInt(vals[1])
	if err != nil {
		return nil, err
	}
	end, err := asInt(vals[2])
	if err != nil {
		return nil, err
	}
	if start < 0 || end < start || end > len(str) {
		return nil, fmt.Errorf("cannot produce substring %v:%v", start, end)
	}
	return str[start:end], nil
}

// `string-length` procedure
func StringLength(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	s, ok := v.(string)
	if !ok {
		return nil, fmt.Errorf("%v is not a string", v)
	}
	return types.Integer(len(s)), nil
}

// Convert arguments to string, using space as a separator
func joinToString(args []any, sep string) string {
	var s []string
	for _, a := range args {
		switch val := a.(type) {
		case string:
			s = append(s, fmt.Sprintf("%v", string(val)))
		default:
			s = append(s, fmt.Sprintf("%v", val))
		}
	}
	return strings.Join(s, sep)
}
