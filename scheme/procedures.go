package scheme

import (
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

var Procedures = eval.ProceduresGetter{
	"-":             Dif,
	"->float":       ToFloat,
	"->int":         ToInt,
	"*":             Mul,
	"/":             Div,
	"//":            IntDiv,
	"%":             Mod,
	"+":             Sum,
	"<":             Lower,
	"=":             Equal,
	">":             Greater,
	"and":           And,
	"begin":         Begin,
	"boolean?":      IsBool,
	"car":           Car,
	"cdr":           Cdr,
	"cond":          Cond,
	"cons":          Cons,
	"debug":         Debug,
	"define":        Define,
	"display":       Display,
	"do":            Do,
	"else":          types.TRUE,
	"eq?":           Eq,
	"error":         Error,
	"eval":          Eval,
	"float?":        IsFloat,
	"go":            Go,
	"if":            If,
	"integer?":      IsInteger,
	"let":           Let,
	"list":          List,
	"load":          Load,
	"map":           Map,
	"nil?":          IsNil,
	"not":           Not,
	"null?":         IsNull,
	"number?":       IsNumber,
	"or":            Or,
	"pair?":         IsPair,
	"procedure?":    IsProcedure,
	"quote":         Quote,
	"set!":          Set,
	"string-length": StringLength,
	"string?":       IsString,
	"string":        ToString,
	"substring":     Substring,
	"symbol?":       IsSymbol,
	"timeit":        Timeit,
}
