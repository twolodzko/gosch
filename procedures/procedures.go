package procedures

import (
	"github.com/twolodzko/gosch/eval"
)

var Procedures = eval.ProceduresGetter{
	"-":             dif,
	"->float":       toFloat,
	"->int":         toInt,
	"*":             mul,
	"/":             div,
	"//":            intDiv,
	"%":             mod,
	"+":             sum,
	"<":             lower,
	"=":             equal,
	">":             greater,
	"and":           and,
	"begin":         begin,
	"boolean?":      isBool,
	"car":           car,
	"cdr":           cdr,
	"cond":          cond,
	"cons":          cons,
	"debug":         debug,
	"define":        define,
	"display":       display,
	"do":            do,
	"else":          elseWord,
	"eq?":           eq,
	"error":         raiseError,
	"eval":          evalFn,
	"float?":        isFloat,
	"go":            goFn,
	"if":            ifFn,
	"integer?":      isInteger,
	"let":           let,
	"list":          list,
	"load":          load,
	"map":           mapFn,
	"nil?":          isNil,
	"not":           not,
	"null?":         isNull,
	"number?":       isNumber,
	"or":            or,
	"pair?":         isPair,
	"procedure?":    isProcedure,
	"quote":         quote,
	"set!":          set,
	"string-length": stringLength,
	"string?":       isString,
	"string":        toString,
	"substring":     substring,
	"symbol?":       isSymbol,
	"timeit":        timeit,
}
