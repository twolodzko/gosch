package macros_test

import (
	"fmt"
	"testing"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

func Test_EvalMacros(t *testing.T) {
	eval.Procedures = scheme.Procedures

	var testCases = []struct {
		macro    string
		input    string
		expected string
	}{
		{
			`
			(define-syntax foo
				(syntax-rules ()
					((_ (x ...) y ...)
					 (list '(x y ...) ...))))
			`,
			"(foo (1 2) 3 4)",
			"((1 3 4) (2 3 4))",
		},
		{
			`
			(define-syntax bar
				(syntax-rules ()
					((_ (x y) ...)
					 (list '(x y) ... '(y x) ... x ... y ...))))
			`,
			"(bar (1 2) (3 4))",
			"((1 2) (3 4) (2 1) (4 3) 1 3 2 4)",
		},
		{
			`
			(define-syntax baz
				(syntax-rules ()
					((_ x ...)
					 (list '(x x ...) ...))))
			`,
			"(baz 1 2 3)",
			"((1 1 2 3) (2 1 2 3) (3 1 2 3))",
		},
		{
			`
			(define-syntax buz
				(syntax-rules ()
					((_ (x ...) y ...)
					 (list '(x y y ...) ...))))
			`,
			"(buz (1 2 3) 4 5 6 7)",
			"((1 4 4 5 6 7) (2 5 4 5 6 7) (3 6 4 5 6 7))",
		},
		// FIXME
		// {
		// 	`
		// 	(define-syntax bar
		// 		(syntax-rules ()
		// 			((_ (x y ...) ...)
		// 			 (list '(x y ...) ...))))
		// 	`,
		// 	"(bar (1 2 3) (4 5 6) (7 8 9))",
		// 	"((1 2 3) (4 5 6) (7 8 9))",
		// },
		{
			`
			(define-syntax foo
				(syntax-rules ()
					((_ x)
					 (lambda (y)
						(list x y)))))
			`,
			"((foo 'a) 'b)",
			"(a b)",
		},
		{
			`
			(define-syntax bar
				(syntax-rules ()
					((_ x y)
					 (let ((z y))
						(list x z)))))
			`,
			"(bar 'a 'b)",
			"(a b)",
		},
		{
			`
			(define-syntax and2
				(syntax-rules ()
					((_) #t)
					((_ e) e)
					((_ e1 e2 ...)
					 (if
						e1
						(and2 e2 ...)
						#f))))
			`,
			"(expand-macro and2 a b c)",
			"(if a (and2 b c) #f)",
		},
		{
			`
			(define-syntax or2
				(syntax-rules ()
					((_ e1 e2)
					 (let ((t e1))
						(if t t e2)))))
			`,
			`
			(let ((t "okay"))
				(or2 #f t))
			`,
			`"okay"`,
		},
		{
			`
			(define-syntax letlist
				(syntax-rules ()
					((_ x y)
					 (let ((x x))
						(let ((y y))
							(list x y))))))
			`,
			`
			(let ((x 2)
				(y 1))
				(letlist y x))
			`,
			"(1 2)",
		},
		{
			`
			(define-syntax swap
				(syntax-rules ()
					((_ x y)
				 	 (let ((tmp x))
						(set! x y)
						(set! y tmp)))))
			`,
			`
			(let ((tmp 5)
				(other 6))
				(swap tmp other)
				(list tmp other))
			`,
			"(6 5)",
		},
		{
			`
			(define-syntax foo
				(syntax-rules ()
					((_ e1 e2 ...)
					 (list e1 e2 ...))))
			`,
			"(foo #t)",
			"(#t)",
		},
		{
			`
			(define-syntax lambd
				(syntax-rules ()
					((_ (arg ...) expr ...)
					 (lambda (arg ...) expr ...))))
			`,
			"((lambd (x) x) 'ok)",
			"ok",
		},
		{
			`
			(define-syntax listgen
				(syntax-rules ()
					((_ x ...)
					 (lambda (y)
						(list y x ...)))))
			`,
			"(expand-macro listgen 1 2 3)",
			"(lambda (g0001) (list g0001 1 2 3))",
		},
		{
			`
			(define-syntax mylet
				(syntax-rules ()
					((_ ((k v) ...) expr ...)
					 ((lambda (k ...) expr ...)
						v ...))))
			`,
			`
			(mylet ((x 1) (y 2))
				(+ x y))
			`,
			"3",
		},
		{
			`
			(define-syntax tuple
				(syntax-rules ()
					((_ v z)
					 ((lambda (x y)
							(let ((y x) (x y))
								(list x y))) v z))))
			`,
			`
			(let ((x 'a) (y 'b))
				(expand-macro tuple x y))
			`,
			"((lambda (g0001 g0002) (let ((g0003 g0001) (g0004 g0002)) (list g0004 g0003))) x y)",
		},
		{
			`
			(define-syntax tuple
				(syntax-rules ()
					((_ v z)
					 ((lambda (x y)
							(let* ((y x) (x y))
								(list x y))) v z))))
			`,
			`
			(let ((x 'a) (y 'b))
				(expand-macro tuple x y))
			`,
			"((lambda (g0001 g0002) (let* ((g0003 g0001) (g0004 g0003)) (list g0004 g0003))) x y)",
		},
		{
			`
			(define-syntax mkmacro
				(syntax-rules ()
					((_ args ...)
					 (macro (x y)
							(quasiquote
								(list
									(unquote x)
									(unquote y)
									args ...))))))
			`,
			"(expand-macro mkmacro 'a 'b)",
			"(macro (g0001 g0002) (quasiquote (list (unquote g0001) (unquote g0002) (quote a) (quote b))))",
		},
		{
			`
			(define-syntax dododoo
				(syntax-rules ()
					((_ init max x ...)
					 (do
						((i init (+ 1 i))
						(lst '() (cons (list i x ...) lst)))
						((> i max) lst)))))
			`,
			"(expand-macro dododoo 1 5 'a 'b 'c)",
			"(do ((g0001 1 (+ 1 g0001)) (g0002 (quote ()) (cons (list g0001 (quote a) (quote b) (quote c)) g0002))) ((> g0001 5) g0002))",
		},
	}

	for _, tt := range testCases {
		gensym.Generator.Reset()
		env := envir.NewEnv()

		_, _, err := eval.EvalString(tt.macro, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}

		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}
		if len(result) != 1 {
			t.Errorf("expected single result, got: %v", result)
			return
		}
		if fmt.Sprintf("%v", result[0]) != tt.expected {
			t.Errorf("expected %q, got: %q", tt.expected, result[0])
		}
	}
}

func Test_Sum(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax sum
		(syntax-rules ()
			((_) 1)
			((_ x) x)
			((_ x y) (+ x y))
			((_ x y ...) (+ x (sum y ...)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	var testCases = []struct {
		input    string
		expected types.Sexpr
	}{
		{"(sum)", types.Integer(1)},
		{"(sum -1)", types.Integer(-1)},
		{"(sum 2 2)", types.Integer(4)},
		{"(sum 1 2 3)", types.Integer(6)},
		{"(sum 1 2 3 4)", types.Integer(10)},
	}

	for _, tt := range testCases {
		result, _, err := eval.EvalString(tt.input, env)
		if err != nil {
			t.Errorf("%s has raised an unexpected error: %v", tt.input, err)
		}
		if len(result) != 1 {
			t.Errorf("for %s expected single result, got: %v", tt.input, result)
		} else if result[0] != tt.expected {
			t.Errorf("for %s expected %v, got: %v", tt.input, tt.expected, result[0])
		}
	}
}

func Test_ValidErrors(t *testing.T) {
	eval.Procedures = scheme.Procedures

	var testCases = []string{
		// FIXME
		// "(define-syntax foo (syntax-rules () ((_ x y x) x)))",
		// "(define-syntax foo (syntax-rules () ((_ x (y x)) x)))",
		"(define-syntax bar (syntax-rules () ((_ x ... y) (list x y ...))))",
		"(define-syntax bar (syntax-rules () ((_ x (... z) y) (list x y ...))))",
		"(define-syntax faz (syntax-rules () ((_ x (...) ...) (list x ...))))",
	}

	for _, input := range testCases {
		env := envir.NewEnv()

		_, _, err := eval.EvalString(input, env)
		if err == nil {
			t.Errorf("%v didn't throw error", input)
		}
	}
}
