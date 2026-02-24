package scheme

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme/primitives"
	"github.com/twolodzko/gosch/scheme/special_forms"
	"github.com/twolodzko/gosch/types"
)

func DefaultEnv() *envir.Env {
	env := envir.NewEnv()
	env.Vars = map[types.Symbol]any{
		"-":             primitives.Sub,
		"->float":       primitives.ToFloat,
		"->int":         primitives.ToInt,
		"*":             primitives.Mul,
		"/":             primitives.Div,
		"//":            primitives.IntDiv,
		"%":             primitives.Mod,
		"+":             primitives.Sum,
		"<":             primitives.Lower,
		"=":             primitives.Equal,
		">":             primitives.Greater,
		"and":           special_forms.And,
		"begin":         eval.PartialEval,
		"boolean?":      primitives.IsBool,
		"car":           primitives.Car,
		"cdr":           primitives.Cdr,
		"cond":          special_forms.Cond,
		"cons":          primitives.Cons,
		"debug":         primitives.Debug,
		"define":        special_forms.Define,
		"display":       primitives.Display,
		"do":            special_forms.Do,
		"else":          true,
		"eq?":           primitives.Eq,
		"error":         primitives.Error,
		"eval":          special_forms.Eval,
		"float?":        primitives.IsFloat,
		"if":            special_forms.If,
		"integer?":      primitives.IsInteger,
		"lambda":        special_forms.NewLambda,
		"let":           special_forms.Let,
		"let*":          special_forms.LetStar,
		"list":          primitives.List,
		"load":          special_forms.Load,
		"newline":       primitives.Newline,
		"not":           special_forms.Not,
		"null?":         primitives.IsNull,
		"number?":       primitives.IsNumber,
		"or":            special_forms.Or,
		"pair?":         primitives.IsPair,
		"procedure?":    primitives.IsProcedure,
		"quasiquote":    special_forms.QuasiQuote,
		"quote":         special_forms.Quote,
		"set!":          special_forms.Set,
		"string-length": primitives.StringLength,
		"string?":       primitives.IsString,
		"string":        primitives.ToString,
		"substring":     primitives.Substring,
		"symbol?":       primitives.IsSymbol,
		"timeit":        primitives.Timeit,
		"unquote":       special_forms.Unquote,
	}
	return env
}
