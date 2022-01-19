package main

import (
	"testing"

	"github.com/google/go-cmp/cmp"
)

func Test_EnvGet(t *testing.T) {
	var testCases = []struct {
		name  string
		value Sexpr
	}{
		{"a", Sexpr{"xxx"}},
		{"b", Sexpr{42}},
	}

	env := NewEnv()
	for _, tt := range testCases {
		env.Set(tt.name, tt.value)
	}

	for _, tt := range testCases {
		result, err := env.Get(tt.name)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.value) {
			t.Errorf("for %q expected %v, got: %v", tt.name, tt.value, result)
		}
	}
}

func Test_EnvGetUnbound(t *testing.T) {
	var values = []struct {
		name  string
		value Sexpr
	}{
		{"a", Sexpr{"xxx"}},
		{"b", Sexpr{42}},
	}

	env := NewEnv()
	for _, tt := range values {
		env.Set(tt.name, tt.value)
	}

	_, err := env.Get("foo")
	if err == nil || err.Error() != "unbound variable foo" {
		t.Errorf("expected unbound variable error, got %v", err)
	}
}

func Test_QuotedSexprEval(t *testing.T) {
	var testCases = []Sexpr{
		{"a"},
		{Pair{}},
		{Pair{quote(Sexpr{"a"}), nil}},
	}

	for _, input := range testCases {
		env := NewEnv()
		result, err := env.Eval(quote(input))
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(input.Value, result.Value) {
			t.Errorf("after evaluating %v its value has changed: %v", input, result)
		}
	}
}

func Test_EvalDoesntMutate(t *testing.T) {
	input := quote(Sexpr{Pair{quote(Sexpr{"a"}), nil}})
	env := NewEnv()
	_, err := env.Eval(input)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(input, quote(Sexpr{Pair{quote(Sexpr{"a"}), nil}})) {
		t.Errorf("%v mutated after eval", input)
	}
}

func Test_EvalExpectError(t *testing.T) {
	var testCases = []Sexpr{
		{"a"},
		{&Pair{Sexpr{"a"}, nil}},
		// FIXME
		// {&Pair{Sexpr{"quote"}, nil}},
	}
	for _, input := range testCases {
		env := NewEnv()
		if _, err := env.Eval(input); err == nil {
			t.Errorf("for %q expected an error", input)
		}
	}
}

func Test_Eval(t *testing.T) {
	var testCases = []struct {
		input    Sexpr
		expected Sexpr
	}{
		{Sexpr{}, Sexpr{}},
		{quote(Sexpr{"a"}), Sexpr{"a"}},
		{
			quote(Sexpr{Pair{Sexpr{"+"}, &Pair{Sexpr{2}, &Pair{Sexpr{2}, nil}}}}),
			Sexpr{Pair{Sexpr{"+"}, &Pair{Sexpr{2}, &Pair{Sexpr{2}, nil}}}},
		},
		{Sexpr{42}, Sexpr{42}},
		{Sexpr{"a"}, Sexpr{"xxx"}},
		{Sexpr{"l"}, Sexpr{Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}}},
		{Sexpr{"b"}, Sexpr{26}},
	}

	env := NewEnv()
	env.Set("a", quote(Sexpr{"xxx"}))
	env.Set("b", Sexpr{"c"})
	env.Set("c", Sexpr{"d"})
	env.Set("d", Sexpr{26})
	env.Set("l", quote(Sexpr{Pair{Sexpr{"1"}, &Pair{Sexpr{"2"}, nil}}}))

	for _, tt := range testCases {
		result, err := env.Eval(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalPair(t *testing.T) {
	var testCases = []struct {
		input    *Pair
		expected Sexpr
	}{
		{
			&Pair{}, Sexpr{&Pair{}},
		},
		{
			&Pair{Sexpr{"car"}, &Pair{quote(Sexpr{&Pair{}}), nil}},
			Sexpr{},
		},
		{
			&Pair{Sexpr{"car"}, &Pair{quote(Sexpr{&Pair{Sexpr{"a"}, nil}}), nil}},
			Sexpr{"a"},
		},
		{
			&Pair{Sexpr{"car"},
				&Pair{quote(Sexpr{&Pair{Sexpr{"a"},
					&Pair{Sexpr{"b"},
						&Pair{Sexpr{"c"}, nil}}}}), nil}},
			Sexpr{"a"},
		},
		{
			&Pair{Sexpr{"cdr"}, &Pair{quote(Sexpr{&Pair{}}), nil}},
			Sexpr{},
		},
		{
			&Pair{Sexpr{"cdr"}, &Pair{quote(Sexpr{&Pair{Sexpr{"a"}, nil}}), nil}},
			Sexpr{},
		},
		{
			&Pair{Sexpr{"cdr"},
				&Pair{quote(Sexpr{&Pair{Sexpr{"a"},
					&Pair{Sexpr{"b"},
						&Pair{Sexpr{"c"}, nil}}}}), nil}},
			Sexpr{&Pair{Sexpr{"b"}, &Pair{Sexpr{"c"}, nil}}},
		},
		{
			&Pair{Sexpr{"null?"}, &Pair{quote(Sexpr{&Pair{}}), nil}},
			Sexpr{Bool(true)},
		},
		{
			&Pair{Sexpr{"null?"}, &Pair{quote(Sexpr{"a"}), nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"null?"}, &Pair{quote(Sexpr{&Pair{Sexpr{"a"}, nil}}), nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"pair?"}, &Pair{quote(Sexpr{&Pair{}}), nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"pair?"}, &Pair{quote(Sexpr{"a"}), nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"pair?"}, &Pair{quote(Sexpr{&Pair{Sexpr{"a"}, nil}}), nil}},
			Sexpr{Bool(true)},
		},
		{
			&Pair{Sexpr{"pair?"}, &Pair{quote(Sexpr{&Pair{Sexpr{"a"}, &Pair{Sexpr{"b"}, nil}}}), nil}},
			Sexpr{Bool(true)},
		},
		{
			&Pair{Sexpr{"cons"}, &Pair{quote(Sexpr{"a"}), &Pair{quote(Sexpr{&Pair{}}), nil}}},
			Sexpr{&Pair{Sexpr{"a"}, nil}},
		},
		{
			&Pair{Sexpr{"cons"}, &Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}},
			Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}},
		},
		{
			&Pair{Sexpr{"list"}, &Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}},
			Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}},
		},
		{
			&Pair{Sexpr{"list"}, nil}, Sexpr{&Pair{}},
		},
		{
			&Pair{Sexpr{"list"}, &Pair{Sexpr{1}, &Pair{Sexpr{2}, &Pair{Sexpr{3}, nil}}}},
			Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, &Pair{Sexpr{3}, nil}}}},
		},
		{
			&Pair{Sexpr{"not"}, &Pair{Sexpr{Bool(false)}, nil}},
			Sexpr{Bool(true)},
		},
		{
			&Pair{Sexpr{"not"}, &Pair{Sexpr{Bool(true)}, nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"not"}, &Pair{Sexpr{3}, nil}},
			Sexpr{Bool(false)},
		},
		{
			&Pair{Sexpr{"quote"}, &Pair{Sexpr{"a"}, nil}},
			Sexpr{"a"},
		},
		{
			&Pair{Sexpr{"quote"}, &Pair{Sexpr{&Pair{Sexpr{"list"}, &Pair{Sexpr{2}, nil}}}, nil}},
			Sexpr{&Pair{Sexpr{"list"}, &Pair{Sexpr{2}, nil}}},
		},
	}

	env := NewEnv()
	for _, tt := range testCases {
		result, err := env.EvalPair(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_EvalArgs(t *testing.T) {
	input := newPair([]Sexpr{quote(Sexpr{"a"}), quote(Sexpr{"b"}), quote(Sexpr{"c"})})
	expected := newPair([]Sexpr{{"b"}, {"c"}})
	env := NewEnv()

	result, err := env.EvalArgs(input)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", input, expected, result)
	}

	// Eval should not mutate the input
	inputCopy := newPair([]Sexpr{quote(Sexpr{"a"}), quote(Sexpr{"b"}), quote(Sexpr{"c"})})
	if !cmp.Equal(input, inputCopy) {
		t.Errorf("input %v has changed to %v", inputCopy, input)
	}
}

func Test_ParseEvalPrint(t *testing.T) {
	var testCases = []struct {
		input    string
		expected string
	}{
		{"()", "()"},
		{"'()", "()"},
		{"'(1 2 3)", "(1 2 3)"},
		{"'(1 (((2)) 3))", "(1 (((2)) 3))"},
		{"(car '())", "<nil>"},
		{"(car '(1 2 3))", "1"},
		{"(car '((a) b c d))", "(a)"},
		{"(cdr '(1 2 3))", "(2 3)"},
		{"(car (cdr '(1 2 3)))", "2"},
		{"(cdr '())", "<nil>"},
		{"(cdr '((a) b c d))", "(b c d)"},
		{"(cdr '(a b))", "b"},
		{"(cons 1 '())", "(1)"},
		{"(cons 1 2)", "(1 2)"},
		{"(cons 1 '(2 3))", "(1 2 3)"},
		{"(cons '() '())", "(())"},
		{"(cons '() '(a b c))", "(() a b c)"},
		{"(cons '(a b c) '())", "((a b c))"},
		{"(list)", "()"},
		{"(list 1)", "(1)"},
		{"(list 1 2 3)", "(1 2 3)"},
		{"(list '(1) 2 3)", "((1) 2 3)"},
		{"(not #t)", "#f"},
		{"(not 3)", "#f"},
		{"(not (list 3))", "#f"},
		{"(not #f)", "#t"},
		{"'a", "a"},
		{"(quote a)", "a"},
		{"(quote (quote a))", "(quote a)"},
	}

	for _, tt := range testCases {
		parser := newParser(tt.input)
		sexprs, err := parser.Read()
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}

		env := NewEnv()
		for _, sexpr := range sexprs {
			result, err := env.Eval(sexpr)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
			}
			if !cmp.Equal(result.String(), tt.expected) {
				t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
			}
		}
	}
}

func Test_AliasToFunction(t *testing.T) {
	expected := Sexpr{&Pair{Sexpr{1}, &Pair{Sexpr{2}, nil}}}

	env := NewEnv()
	env.vars["my-list"] = Sexpr{"list"}

	result, err := env.Eval(
		Sexpr{&Pair{Sexpr{"my-list"},
			&Pair{Sexpr{1},
				&Pair{Sexpr{2}, nil}}}})
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("expected: %v, got %v", expected, result)
	}
}

func Test_Define(t *testing.T) {
	env := NewEnv()

	_, err := env.Eval(
		Sexpr{&Pair{Sexpr{"define"},
			&Pair{Sexpr{"x"},
				&Pair{Sexpr{42}, nil}}}})
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}

	result, ok := env.vars["x"]
	if !ok || result != (Sexpr{42}) {
		t.Errorf("variable was not set correctly: %v", result)
	}
}

func Test_QuoteDoesntMutate(t *testing.T) {
	example := quote(Sexpr{"a"})
	env := NewEnv()
	result, err := env.Eval(example)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, Sexpr{"a"}) {
		t.Errorf("expected %v, got %v", Sexpr{"a"}, result)
	}
	if !cmp.Equal(example, quote(Sexpr{"a"})) {
		t.Errorf("object mutated %v", example)
	}
}
