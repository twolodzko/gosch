package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

type (
	Primitive         = func(*types.Pair) (types.Sexpr, error)
	Procedure         = func(*types.Pair, *envir.Env) (types.Sexpr, error)
	TailCallOptimized = func(*types.Pair, *envir.Env) (types.Sexpr, *envir.Env, error)
)

func isCallable(obj types.Sexpr) bool {
	switch obj.(type) {
	case Procedure, Primitive, TailCallOptimized, Lambda:
		return true
	default:
		return false
	}
}

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
		return quote, true
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
		return func(args *types.Pair) (types.Sexpr, error) {
			return types.String(toString(args, "")), nil
		}, true
	case "substring":
		return substring, true
	case "string-length":
		return stringLength, true
	case "display":
		return display, true
	case "error":
		return raiseError, true
	case "load":
		return load, true
	case "debug":
		return debug, true
	default:
		return nil, false
	}
}

func and(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		test, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if !types.IsTrue(test) {
			return types.Bool(false), nil
		}
		head = head.Next
	}
	return types.Bool(true), nil
}

func or(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args.This == nil {
		return types.Bool(true), nil
	}
	head := args
	for head != nil {
		test, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		if types.IsTrue(test) {
			return types.Bool(true), nil
		}
		head = head.Next
	}
	return types.Bool(false), nil
}

func quote(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	return args.This, nil
}

func define(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	switch name := args.This.(type) {
	case types.Symbol:
		val, err := Eval(args.Next.This, env)
		if err != nil {
			return nil, err
		}
		env.Set(name, val)
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}

func set(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}

	val, err := Eval(args.Next.This, env)
	if err != nil {
		return nil, err
	}

	switch name := args.This.(type) {
	case types.Symbol:
		if localEnv, ok := env.FindEnv(name); ok {
			localEnv.Set(name, val)
		} else {
			env.Set(name, val)
		}
		return val, nil
	default:
		return nil, fmt.Errorf("%v is not a valid variable name", args.This)
	}
}

func load(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	if args == nil {
		return nil, errors.New("wrong number of arguments")
	}
	head, err := Eval(args.This, env)
	if err != nil {
		return nil, err
	}
	path, ok := head.(types.String)
	if !ok {
		return nil, fmt.Errorf("invalid path: %v", head)
	}
	sexprs, err := LoadEval(string(path), env)
	if err != nil {
		return nil, err
	}
	if len(sexprs) > 0 {
		return sexprs[len(sexprs)-1], nil
	}
	return nil, nil
}
