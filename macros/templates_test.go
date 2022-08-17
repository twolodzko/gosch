package macros

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func Test_Template(t *testing.T) {
	var testCases = []struct {
		template SexprTemplate
		mapping  Mappings
		expected types.Sexpr
	}{
		{
			SexprTemplate{&types.Pair{}},
			Mappings{},
			&types.Pair{},
		},
		{
			SexprTemplate{types.FALSE},
			Mappings{},
			types.FALSE,
		},
		{
			SexprTemplate{types.Symbol("x")},
			Mappings{"x": types.Integer(42)},
			types.Integer(42),
		},
		{
			SexprTemplate{types.NewPair(types.Symbol("x"), types.Symbol("y"))},
			Mappings{"x": types.Integer(42)},
			types.NewPair(types.Integer(42), types.Symbol("y")),
		},
		{
			SexprTemplate{types.NewPair(types.Symbol("x"), types.NewPair(types.Symbol("x"), types.Symbol("y")))},
			Mappings{types.Symbol("x"): types.Symbol("z")},
			types.NewPair(types.Symbol("z"), types.NewPair(types.Symbol("z"), types.Symbol("y"))),
		},
		{
			SexprTemplate{types.NewPair(EllipsisTemplate{types.Symbol("x"), []types.Symbol{"x"}}, nil)},
			Mappings{"x": EllipsisVar{1, 2, 3}},
			types.PairFromArray([]types.Sexpr{1, 2, 3}),
		},
		{
			SexprTemplate{types.NewPair(EllipsisTemplate{types.NewPair(types.Symbol("x"), types.Symbol("y")), []types.Symbol{"x", "y"}}, nil)},
			Mappings{"x": EllipsisVar{1, 2, 3}, "y": EllipsisVar{4, 5, 6}},
			types.PairFromArray([]types.Sexpr{types.NewPair(1, 4), types.NewPair(2, 5), types.NewPair(3, 6)}),
		},
		{
			SexprTemplate{
				types.NewPair(
					EllipsisTemplate{
						types.PairFromArray(
							[]types.Sexpr{
								types.Symbol("list"),
								types.Symbol("x"),
								EllipsisTemplate{
									types.Symbol("y"),
									[]types.Symbol{"y"},
								},
							},
						),
						[]types.Symbol{"x"}},
					nil,
				),
			},
			Mappings{"x": EllipsisVar{1, 2, 3}, "y": EllipsisVar{"a", "b"}},
			types.PairFromArray([]types.Sexpr{
				types.PairFromArray([]types.Sexpr{"list", 1, "a", "b"}),
				types.PairFromArray([]types.Sexpr{"list", 2, "a", "b"}),
				types.PairFromArray([]types.Sexpr{"list", 3, "a", "b"}),
			}),
		},
	}

	for _, tt := range testCases {
		result, err := tt.template.Transform(tt.mapping)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for template %v and mapping %v expected %v, got %v", tt.template, tt.mapping, tt.expected, result)
		}
	}
}
