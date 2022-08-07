package macros

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/parser"
	"github.com/twolodzko/gosch/types"
)

func Test_Transform(t *testing.T) {
	var testCases = []struct {
		mapping  Mapping
		template *types.Pair
		expected *types.Pair
	}{
		{
			Mapping{},
			&types.Pair{},
			&types.Pair{},
		},
		{
			Mapping{"x": "y"},
			&types.Pair{},
			&types.Pair{},
		},
		{
			Mapping{"x": "y"},
			types.NewPair("x", nil),
			types.NewPair("y", nil),
		},
		{
			Mapping{"x": &types.Pair{}},
			types.NewPair("x", nil),
			types.NewPair(&types.Pair{}, nil),
		},
		{
			Mapping{"x": "a", "y": "b", "z": "c"},
			types.PairFromArray([]types.Sexpr{"list", "x", "z"}),
			types.PairFromArray([]types.Sexpr{"list", "a", "c"}),
		},
		{
			Mapping{"x": "a", "y": "b", "z": "c"},
			types.PairFromArray([]types.Sexpr{"car", parser.Quote(types.PairFromArray([]types.Sexpr{"x", "y", "m", "n", "y"}))}),
			types.PairFromArray([]types.Sexpr{"car", parser.Quote(types.PairFromArray([]types.Sexpr{"a", "b", "m", "n", "b"}))}),
		},
		// {
		// 	Mapping{"x": "a", "...": types.NewPair("b", "c")},
		// 	types.PairFromArray([]types.Sexpr{"list", "..."}),
		// 	types.PairFromArray([]types.Sexpr{"list", "b", "c"}),
		// },
	}

	for _, tt := range testCases {
		result := tt.mapping.Transform(tt.template)

		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for mapping %v and template %v expected %v, got %v", tt.mapping, tt.template, tt.expected, result)
		}
	}
}
