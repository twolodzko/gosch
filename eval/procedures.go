package eval

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type (
	Primitive = func(*types.Pair) (types.Any, error)
	Procedure = func(*types.Pair, *envir.Env) (types.Any, error)
)

func procedure(name string) (Procedure, bool) {
	var fn Primitive
	switch name {
	case "car":
		fn = car
	case "cdr":
		fn = cdr
	case "cons":
		fn = cons
	case "list":
		fn = list
	case "not":
		fn = not
	case "eq?":
		fn = eq
	case "and":
		fn = and
	case "or":
		fn = or
	case "+":
		fn = sum
	case "-":
		fn = dif
	case "*":
		fn = mul
	case "/":
		fn = div
	case "%":
		fn = mod
	case "=":
		fn = equal
	case "<":
		fn = lower
	case ">":
		fn = higher
	case "define":
		return define, true
	case "set!":
		return set, true
	case "quote":
		return func(args *types.Pair, env *envir.Env) (types.Any, error) {
			return args.This, nil
		}, true
	case "lambda":
		return newLambda, true
	case "null?":
		fn = isNull
	case "pair?":
		fn = isPair
	case "number?":
		fn = isNumber
	case "boolean?":
		fn = isBool
	case "symbol?":
		fn = isSymbol
	case "nil?":
		fn = isNil
	default:
		return nil, false
	}
	return primitiveWrapper(fn), true
}

func primitiveWrapper(fn Primitive) Procedure {
	return func(args *types.Pair, env *envir.Env) (types.Any, error) {
		args, err := evalArgs(args, env)
		if err != nil {
			return nil, err
		}
		return fn(args)
	}
}

func evalArgs(pair *types.Pair, env *envir.Env) (*types.Pair, error) {
	var (
		head *types.Pair
		args []types.Any
	)
	head = pair
	for head != nil {
		sexpr, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
		head = head.Next
	}
	// TODO: avoid re-packing
	return types.PairFromArray(args), nil
}
