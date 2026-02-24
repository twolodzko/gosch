package primitives

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `number?` procedure
func IsNumber(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	switch v.(type) {
	case types.Integer, types.Float:
		return true, nil
	default:
		return false, nil
	}
}

// `integer?` procedure
func IsInteger(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	switch v.(type) {
	case types.Integer:
		return true, nil
	default:
		return false, nil
	}
}

// `float?` procedure
func IsFloat(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	switch v.(type) {
	case types.Float:
		return true, nil
	default:
		return false, nil
	}
}

// `->int` procedure
func ToInt(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	switch num := v.(type) {
	case types.Integer:
		return num, nil
	case types.Float:
		return types.Integer(num), nil
	default:
		return nil, &types.NaN{Val: num}
	}
}

// `->float` procedure
func ToFloat(args any, env *envir.Env) (any, error) {
	v, err := eval.EvalOne(args, env)
	if err != nil {
		return nil, err
	}
	switch num := v.(type) {
	case types.Integer:
		return types.Float(num), nil
	case types.Float:
		return num, nil
	default:
		return nil, &types.NaN{Val: num}
	}
}

// `+` procedure
func Sum(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(0), nil
	}
	result, ok := vals[0].(types.Arith)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.Add(vals[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// `-` procedure
func Sub(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(0), nil
	}
	result, ok := vals[0].(types.Arith)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	if len(vals) == 1 {
		switch num := result.(type) {
		case types.Integer:
			return -num, nil
		case types.Float:
			return -num, nil
		}
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.Sub(vals[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// `*` procedure
func Mul(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(1), nil
	}
	result, ok := vals[0].(types.Arith)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.Mul(vals[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// `/` procedure
func Div(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(1), nil
	}
	result, ok := vals[0].(types.Arith)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	if len(vals) == 1 {
		return types.Float(1.0).Div(result)
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.Div(vals[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// `%` procedure
func Mod(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(1), nil
	}
	result, ok := vals[0].(types.Arith)
	if !ok {
		return nil, eval.NaN{Val: vals[0]}
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.Mod(vals[i])
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

// `//` procedure
func IntDiv(args any, env *envir.Env) (any, error) {
	vals, err := eval.ListMapEval(args, env)
	if err != nil {
		return nil, err
	}
	if len(vals) == 0 {
		return types.Integer(1), nil
	}
	result, ok := vals[0].(types.Integer)
	if !ok {
		return types.Integer(1), fmt.Errorf("%v is not an integer", vals[0])
	}
	if len(vals) == 1 {
		return types.Integer(1).IntDiv(result)
	}
	for i := 1; i < len(vals); i++ {
		result, err = result.IntDiv(vals[i])
		if err != nil {
			return types.Integer(1), err
		}
	}
	return result, nil
}
