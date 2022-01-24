package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type (
	Primitive    = func(*types.Pair) (types.Any, error)
	Procedure    = func(*types.Pair, *envir.Env) (types.Any, error)
	TcoProcedure = func(*types.Pair, *envir.Env) (types.Any, *envir.Env, error)
)

func procedure(name types.Symbol) (interface{}, bool) {
	switch name {
	case "car":
		return car, true
	case "cdr":
		return cdr, true
	case "cons":
		return cons, true
	case "list":
		return list, true
	case "not":
		return not, true
	case "eq?":
		return eq, true
	case "and":
		return and, true
	case "or":
		return or, true
	case "=":
		return equal, true
	case "<":
		return lower, true
	case ">":
		return higher, true
	case "define":
		return define, true
	case "set!":
		return set, true
	case "quote":
		return func(args *types.Pair, env *envir.Env) (types.Any, error) {
			if args == nil {
				return nil, errors.New("wrong number of arguments")
			}
			return args.This, nil
		}, true
	case "lambda":
		return newLambda, true
	case "let":
		return let, true
	case "if":
		return ifFn, true
	case "cond":
		return cond, true
	case "else":
		return types.Bool(true), true
	case "begin":
		return partialEval, true
	case "null?":
		return isNull, true
	case "pair?":
		return isPair, true
	case "number?":
		return isNumber, true
	case "boolean?":
		return isBool, true
	case "string?":
		return isString, true
	case "symbol?":
		return isSymbol, true
	case "procedure?":
		return isProcedure, true
	case "nil?":
		return isNil, true
	case "+":
		return sum, true
	case "-":
		return dif, true
	case "*":
		return mul, true
	case "/":
		return div, true
	case "%":
		return mod, true
	case "string":
		return func(args *types.Pair) (types.Any, error) {
			return types.String(toString(args, "")), nil
		}, true
	case "display":
		return display, true
	case "newline":
		return func(args *types.Pair) (types.Any, error) {
			fmt.Println()
			return nil, nil
		}, true
	case "error":
		return raiseError, true
	case "load":
		return load, true
	case "debug":
		return func(args *types.Pair) (types.Any, error) {
			if args == nil {
				DEBUG = true
				return nil, nil
			}
			if args.HasNext() {
				return nil, errors.New("wrong number of arguments")
			}
			DEBUG = bool(types.IsTrue(args.This))
			return nil, nil
		}, true
	default:
		return nil, false
	}
}

func evalArgs(pair *types.Pair, env *envir.Env) (*types.Pair, error) {
	if pair == nil {
		return nil, nil
	}
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
