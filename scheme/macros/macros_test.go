package macros_test

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/scheme"
	"github.com/twolodzko/gosch/scheme/macros/gensym"
	"github.com/twolodzko/gosch/types"
)

func Test_EllipsisExpansion1(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax foo
		(syntax-rules ()
			((_ (x ...) y ...)
			 (list '(x y ...) ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err := eval.EvalString("(foo (1 2) 3 4)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := "((1 3 4) (2 3 4))"
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_EllipsisExpansion2(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax bar
		(syntax-rules ()
			((_ (x y) ...)
			 (list '(x y) ... '(y x) ... x ... y ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err := eval.EvalString("(bar (1 2) (3 4))", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := "((1 2) (3 4) (2 1) (4 3) 1 3 2 4)"
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_EllipsisExpansion3(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax baz
		(syntax-rules ()
			((_ x ...)
			(list '(x x ...) ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err := eval.EvalString("(baz 1 2 3)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := "((1 1 2 3) (2 1 2 3) (3 1 2 3))"
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

// FIXME
// func Test_EllipsisExpansion4(t *testing.T) {
// 	eval.Procedures = scheme.Procedures
// 	env := envir.NewEnv()

// 	macro := `
// 	(define-syntax bar
// 		(syntax-rules ()
// 			((_ (x y ...) ...)
// 			 (list '(x y ...) ...))))
// 	`

// 	_, _, err := eval.EvalString(macro, env)
// 	if err != nil {
// 		t.Errorf("unexpected error: %v", err)
// 		return
// 	}

// 	result, _, err := eval.EvalString("(bar (1 2 3) (4 5 6) (7 8 9))", env)
// 	if err != nil {
// 		t.Errorf("unexpected error: %v", err)
// 		return
// 	}
// 	if len(result) != 1 {
// 		t.Errorf("expected single result, got: %v", result)
// 		return
// 	}
// 	expected := "((1 2 3) (4 5 6) (7 8 9))"
// 	if fmt.Sprintf("%s", result[0]) != expected {
// 		t.Errorf("expected %q, got: %q", expected, result[0])
// 	}
// }

func Test_EllipsisExpansion5(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
  	(define-syntax buz
		(syntax-rules ()
			((_ (x ...) y ...)
			 (list '(x y y ...) ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	result, _, err := eval.EvalString("(buz (1 2 3) 4 5 6 7)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}
	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := "((1 4 4 5 6 7) (2 5 4 5 6 7) (3 6 4 5 6 7))"
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
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

func Test_Lambda(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax foo
		(syntax-rules ()
			((_ x)
			 (lambda (y)
			 	(list x y)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "((foo 'a) 'b)"
	expected := types.NewPair("a", "b")

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_Let(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax bar
		(syntax-rules ()
			((_ x y)
			 (let ((z y))
			 	(list x z)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "(bar 'a 'b)"
	expected := types.NewPair("a", "b")

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}

}

func Test_And2Expand(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax and2
		(syntax-rules ()
			((_) #t)
			((_ e) e)
			((_ e1 e2 ...)
			 (if
				e1
				(and2 e2 ...)
				#f))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, _, err := eval.EvalString("(expand-macro and2 a b c)", env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := "(if a (and2 b c) #f)"
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_Or2(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax or2
		(syntax-rules ()
			((_ e1 e2)
			 (let ((t e1))
			 	(if t t e2)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := `
	(let ((t "okay"))
		(or2 #f t))
	`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := types.String("okay")
	if result[0] != expected {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_DoubleLet(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax letlist
		(syntax-rules ()
			((_ x y)
			 (let ((x x))
			 	(let ((y y))
			 		(list x y))))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := `
	(let ((x 2)
		  (y 1))
		(letlist y x))
	`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := types.NewPair(types.Integer(1), types.Integer(2))
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_Swap(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax swap
		(syntax-rules ()
			((_ x y)
			 (let ((tmp x))
			 	(set! x y)
				(set! y tmp)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := `
	(let ((tmp 5)
		  (other 6))
		(swap tmp other)
		(list tmp other))
	`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := types.NewPair(types.Integer(6), types.Integer(5))
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_MyList(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax foo
		(syntax-rules ()
			((_ e1 e2 ...)
			 (list e1 e2 ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	input := `(foo #t)`

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	if len(result) != 1 {
		t.Errorf("expected single result, got: %v", result)
		return
	}
	expected := types.NewPair(types.TRUE, nil)
	if !cmp.Equal(result[0], expected) {
		t.Errorf("expected %q, got: %q", expected, result[0])
	}
}

func Test_LambdaWrapper(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax lambd
		(syntax-rules ()
			((_ (arg ...) expr ...)
			 (lambda (arg ...) expr ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "((lambd (x) x) 'ok)"
	expected := "ok"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_LambdaGenerator(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax listgen
		(syntax-rules ()
			((_ x ...)
			 (lambda (y)
				(list y x ...)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "(expand-macro listgen 1 2 3)"
	expected := "(lambda (g0001) (list g0001 1 2 3))"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_MyLet(t *testing.T) {
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax mylet
		(syntax-rules ()
			((_ ((k v) ...) expr ...)
			 ((lambda (k ...) expr ...)
			 	v ...))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := `
	(mylet ((x 1) (y 2))
		(+ x y))
	`
	expected := types.Integer(3)

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if !cmp.Equal(result[0], expected) {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_Shawdowing(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax tuple
		(syntax-rules ()
			((_ v z)
			((lambda (x y)
					(let ((y x) (x y))
						(list x y))) v z))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := `
	(let ((x 'a) (y 'b))
		(expand-macro tuple x y))
	`
	expected := "((lambda (g0001 g0002) (let ((g0003 g0001) (g0004 g0002)) (list g0004 g0003))) x y)"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_LetStar(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax tuple
		(syntax-rules ()
			((_ v z)
			((lambda (x y)
					(let* ((y x) (x y))
						(list x y))) v z))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := `
	(let ((x 'a) (y 'b))
		(expand-macro tuple x y))
	`
	expected := "((lambda (g0001 g0002) (let* ((g0003 g0001) (g0004 g0003)) (list g0004 g0003))) x y)"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_LispMacrosExpand(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax mkmacro
		(syntax-rules ()
			((_ args ...)
			 (macro (x y)
					(quasiquote
						(list
							(unquote x)
							(unquote y)
							args ...))))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "(expand-macro mkmacro 'a 'b)"
	expected := "(macro (g0001 g0002) (quasiquote (list (unquote g0001) (unquote g0002) (quote a) (quote b))))"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
	}
}

func Test_DoExpand(t *testing.T) {
	gensym.Generator.Reset()
	eval.Procedures = scheme.Procedures
	env := envir.NewEnv()

	macro := `
	(define-syntax dododoo
		(syntax-rules ()
			((_ init max x ...)
			(do
				((i init (+ 1 i))
				(lst '() (cons (list i x ...) lst)))
				((> i max) lst)))))
	`

	_, _, err := eval.EvalString(macro, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
		return
	}

	input := "(expand-macro dododoo 1 5 'a 'b 'c)"
	expected := "(do ((g0001 1 (+ 1 g0001)) (g0002 (quote ()) (cons (list g0001 (quote a) (quote b) (quote c)) g0002))) ((> g0001 5) g0002))"

	result, _, err := eval.EvalString(input, env)
	if err != nil {
		t.Errorf("%s has raised an unexpected error: %v", input, err)
		return
	}
	if len(result) != 1 {
		t.Errorf("for %s expected single result, got: %v", input, result)
		return
	}
	if fmt.Sprintf("%s", result[0]) != expected {
		t.Errorf("for %s expected %v, got: %v", input, expected, result[0])
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
