package scheme

import (
	"fmt"
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func Test_NewLambda(t *testing.T) {
	eval.Procedures = Procedures
	env := envir.NewEnv()
	expected := Lambda{
		[]types.Symbol{"x", "y"},
		types.MakePair(types.Symbol("+"), types.MakePair(types.MakePair(types.Symbol("x"), types.MakePair(types.Symbol("y"), nil)), nil)),
		env,
	}
	result, err := NewLambda(
		types.MakePair(
			types.MakePair(types.Symbol("x"), types.MakePair(types.Symbol("y"), nil)),
			types.MakePair(types.Symbol("+"), types.MakePair(types.MakePair(types.Symbol("x"), types.MakePair(types.Symbol("y"), nil)), nil)),
		),
		env,
	)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}

func Test_SimpleLambdas(t *testing.T) {
	eval.Procedures = Procedures
	var testCases = []struct {
		input    string
		expected string
	}{
		{"()", "()"},
		{"((lambda () 42))", "42"},
		{"(lambda (x) x)", "(lambda (x) x)"},
		{"((lambda (x) x) 3)", "3"},
		// lambda calculus
		// true
		{"((lambda (x y) x) #t #f)", "#t"},
		// negation of true
		{"((lambda (x) (x #f #t)) (lambda (x y) x))", "#f"},
		// negation of false
		{"((lambda (x) (x #f #t)) (lambda (x y) y))", "#t"},
		// true and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#f"},
		// false and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#f"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
			return
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if !cmp.Equal(fmt.Sprintf("%v", result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func Test_LambdaCalculus(t *testing.T) {
	// See: https://brilliant.org/wiki/lambda-calculus/
	eval.Procedures = Procedures

	var testCases = []struct {
		input    string
		expected string
	}{
		// true
		{"((lambda (x y) x) #t #f)", "#t"},
		// false
		{"((lambda (x y) y) #t #f)", "#f"},
		// negation of true
		{"(((lambda (x) (x (lambda (x y) y) (lambda (x y) x))) (lambda (x y) x)) #t #f)", "#f"},
		// negation of false
		{"(((lambda (x) (x (lambda (x y) y) (lambda (x y) x))) (lambda (x y) y)) #t #f)", "#t"},
		// true and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#f"},
		// false and true
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#f"},
		// false and false
		{"(((lambda (x y) (x y (lambda (x y) y))) (lambda (x y) y) (lambda (x y) y)) #t #f)", "#f"},
		// true or true
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) x) (lambda (x y) x)) #t #f)", "#t"},
		// true or false
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) x) (lambda (x y) y)) #t #f)", "#t"},
		// false or true
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) y) (lambda (x y) x)) #t #f)", "#t"},
		// false or false
		{"(((lambda (x y) (x (lambda (x y) x) y)) (lambda (x y) y) (lambda (x y) y)) #t #f)", "#f"},
	}

	for _, tt := range testCases {
		parser := parser.NewParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("for %v got an unexpected error: %v", tt.input, err)
			return
		}

		for _, sexpr := range sexprs {
			env := envir.NewEnv()
			result, err := eval.Eval(sexpr, env)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if !cmp.Equal(fmt.Sprintf("%v", result), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func Test_SymbolsPairToSlice(t *testing.T) {

	var testCases = []struct {
		input    *types.Pair
		expected []types.Symbol
	}{
		{
			&types.Pair{},
			nil,
		},
		{
			types.MakePair(types.Symbol("a"), nil),
			[]types.Symbol{"a"},
		},
		{
			types.MakePair(types.Symbol("a"), types.Symbol("b")),
			[]types.Symbol{"a", "b"},
		},
		{
			types.PairFromArray([]types.Sexpr{"a", "b", "c"}),
			[]types.Symbol{"a", "b", "c"},
		},
	}

	for _, tt := range testCases {
		result, err := SymbolsPairToSlice(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}
