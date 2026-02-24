package special_forms

// FIXME

// import (
// 	"testing"

// 	"github.com/google/go-cmp/cmp"
// 	"github.com/twolodzko/gosch/types"
// )

// func TestExtractSymbols(t *testing.T) {

// 	var testCases = []struct {
// 		input    any
// 		expected []types.Symbol
// 	}{
// 		{
// 			types.List(types.Symbol("a")),
// 			[]types.Symbol{"a"},
// 		},
// 		{
// 			types.Cons(types.Symbol("a"), types.Symbol("b")),
// 			[]types.Symbol{"a", "b"},
// 		},
// 		{
// 			types.Cons("a", "b", "c"),
// 			[]types.Symbol{"a", "b", "c"},
// 		},
// 	}

// 	for _, tt := range testCases {
// 		result, err := extractSymbols(tt.input)
// 		if err != nil {
// 			t.Errorf("unexpected error: %v", err)
// 		}
// 		if !cmp.Equal(result, tt.expected) {
// 			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
// 		}
// 	}
// }
