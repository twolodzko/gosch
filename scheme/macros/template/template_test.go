package template

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/scheme/macros/pattern"
	"github.com/twolodzko/gosch/types"
)

func Test_Parse(t *testing.T) {
	var testCases = []struct {
		input    types.Sexpr
		expected types.Sexpr
	}{
		// (x ...)
		{
			types.NewPair(types.Symbol("x"), types.Symbol(pattern.Ellipsis)),
			types.NewPair(EllipsisSymbol("x"), nil),
		},
		// ((x) ...)
		{
			types.NewPair(types.NewPair(types.Symbol("x"), nil), types.Symbol(pattern.Ellipsis)),
			types.NewPair(EllipsisPair(*types.NewPair(types.Symbol("x"), nil)), nil),
		},
		// ((x y ...) ...)
		{
			types.NewPair(types.PairFromArray([]types.Sexpr{types.Symbol("x"), types.Symbol("y"), types.Symbol("...")}), types.Symbol(pattern.Ellipsis)),
			types.NewPair(EllipsisPair(*types.NewPair(types.Symbol("x"), EllipsisSymbol("y"))), nil),
		},
	}

	for _, tt := range testCases {
		result, err := Parse(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
			return
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for template %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_Transform(t *testing.T) {
	var testCases = []struct {
		template types.Sexpr
		mapping  mapping.Mapping
		expected types.Sexpr
	}{
		{
			// ()
			&types.Pair{},
			mapping.Mapping{},
			&types.Pair{},
		},
		{
			// #f
			types.FALSE,
			mapping.Mapping{},
			types.FALSE,
		},
		{
			// x
			types.Symbol("x"),
			mapping.Mapping{"x": types.Integer(42)},
			types.Integer(42),
		},
		{
			// (x y)
			types.NewPair(types.Symbol("x"), types.Symbol("y")),
			mapping.Mapping{"x": types.Integer(42)},
			types.NewPair(types.Integer(42), types.Symbol("y")),
		},
		{
			// (x (x y))
			types.NewPair(types.Symbol("x"), types.NewPair(types.Symbol("x"), types.Symbol("y"))),
			mapping.Mapping{types.Symbol("x"): types.Symbol("z")},
			types.NewPair(types.Symbol("z"), types.NewPair(types.Symbol("z"), types.Symbol("y"))),
		},
		{
			// (x ...)
			types.NewPair(EllipsisSymbol("x"), nil),
			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}},
			types.PairFromArray([]types.Sexpr{1, 2, 3}),
		},
		{
			// ('x ...)
			types.NewPair(EllipsisPair(*(parser.Quote(types.Symbol("x")).(*types.Pair))), nil),
			mapping.Mapping{"x": pattern.EllipsisVar{"a", "b", "c"}},
			types.PairFromArray([]types.Sexpr{parser.Quote("a"), parser.Quote("b"), parser.Quote("c")}),
		},
		{
			// (x ... y)
			types.PairFromArray([]types.Sexpr{EllipsisSymbol("x"), types.Symbol("y")}),
			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}},
			types.PairFromArray([]types.Sexpr{1, 2, 3, types.Symbol("y")}),
		},
		{
			// ((x y) ...)
			types.NewPair(EllipsisPair(*types.NewPair(types.Symbol("x"), types.Symbol("y"))), nil),
			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}, "y": pattern.EllipsisVar{4, 5, 6}},
			types.PairFromArray([]types.Sexpr{types.NewPair(1, 4), types.NewPair(2, 5), types.NewPair(3, 6)}),
		},
		{
			// ('(x y) ...)
			types.NewPair(EllipsisPair(*(parser.Quote(types.NewPair(types.Symbol("x"), types.Symbol("y"))).(*types.Pair))), nil),
			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}, "y": pattern.EllipsisVar{4, 5, 6}},
			types.PairFromArray([]types.Sexpr{parser.Quote(types.NewPair(1, 4)), parser.Quote(types.NewPair(2, 5)), parser.Quote(types.NewPair(3, 6))}),
		},
		{
			// ((x y) ...) with y != EllipsisVar
			types.NewPair(EllipsisPair(*types.NewPair(types.Symbol("x"), types.Symbol("y"))), nil),
			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}, "y": types.TRUE},
			types.PairFromArray([]types.Sexpr{types.NewPair(1, types.TRUE), types.NewPair(2, types.TRUE), types.NewPair(3, types.TRUE)}),
		},
		{
			// ((list x y ...) ...)

			types.NewPair(
				EllipsisPair(
					*types.PairFromArray(
						[]types.Sexpr{
							types.Symbol("list"),
							types.Symbol("x"),
							EllipsisSymbol("y"),
						},
					),
				),
				nil,
			),

			mapping.Mapping{"x": pattern.EllipsisVar{1, 2, 3}, "y": pattern.EllipsisVar{"a", "b"}},
			types.PairFromArray([]types.Sexpr{
				types.PairFromArray([]types.Sexpr{"list", 1, "a", "b"}),
				types.PairFromArray([]types.Sexpr{"list", 2, "a", "b"}),
				types.PairFromArray([]types.Sexpr{"list", 3, "a", "b"}),
			}),
		},
	}

	for _, tt := range testCases {
		result, err := Transform(tt.template, tt.mapping)
		if err != nil {
			t.Errorf("unexpected error: %s", err)
			return
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for template %v and mapping %v expected %v, got %v", tt.template, tt.mapping, tt.expected, result)
		}
	}
}