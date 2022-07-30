package procedures

import (
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

func isCallable(obj types.Sexpr) bool {
	switch obj.(type) {
	case eval.Procedure, eval.Primitive, eval.TailCallOptimized, eval.Lambda:
		return true
	default:
		return false
	}
}

func Procedures(name types.Symbol) (interface{}, bool) {
	switch name {
	case "quote":
		return quote, true
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
		return greater, true
	case "define":
		return define, true
	case "set!":
		return set, true
	case "eval":
		return evalFn, true
	case "let":
		return let, true
	case "if":
		return ifFn, true
	case "cond":
		return cond, true
	case "else":
		return elseWord, true
	case "begin":
		return begin, true
	case "do":
		return do, true
	case "null?":
		return isNull, true
	case "pair?":
		return isPair, true
	case "number?":
		return isNumber, true
	case "integer?":
		return isInteger, true
	case "float?":
		return isFloat, true
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
	case "//":
		return intDiv, true
	case "->float":
		return toFloat, true
	case "->int":
		return toInt, true
	case "string":
		return toString, true
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
	case "map":
		return mapFn, true
	case "go":
		return goFn, true
	case "timeit":
		return timeit, true
	default:
		return nil, false
	}
}
