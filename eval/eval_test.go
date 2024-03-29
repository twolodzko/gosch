package eval

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func Test_EvalEach(t *testing.T) {
	input := types.NewPair(parser.Quote("a"), parser.Quote("b"))
	expected := types.NewPair("a", "b")

	Procedures = ProceduresGetter{
		"quote": func(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
			if args == nil {
				return nil, ErrBadArgNumber
			}
			return args.This, nil
		},
	}
	env := envir.NewEnv()

	result, _, err := EvalEach(input, env)
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
	if !cmp.Equal(result, expected) {
		t.Errorf("for %v expected %v, got %v", input, expected, result)
	}

	// Eval should not mutate the input
	inputCopy := types.NewPair(parser.Quote("a"), parser.Quote("b"))
	if !cmp.Equal(input, inputCopy) {
		t.Errorf("input %v has changed to %v", inputCopy, input)
	}
}
