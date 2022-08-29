package special_forms

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func Test_symbolsPairToSlice(t *testing.T) {

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
			types.PairFromArray("a", "b", "c"),
			[]types.Symbol{"a", "b", "c"},
		},
	}

	for _, tt := range testCases {
		result, err := symbolsPairToSlice(tt.input)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}
