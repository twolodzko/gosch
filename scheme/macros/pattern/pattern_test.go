package pattern

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

func Test_FromPair(t *testing.T) {

	var testCases = []struct {
		input    *types.Pair
		literals []types.Symbol
		expected *Pair
	}{
		{
			// ()
			types.PairFromArray([]types.Sexpr{}),
			nil,
			&Pair{},
		},
		{
			// (x)
			types.PairFromArray([]types.Sexpr{"x"}),
			nil,
			&Pair{[]Pattern{&Identifier{"x", false}}, false},
		},
		{
			// (#t)
			types.PairFromArray([]types.Sexpr{types.Bool(true)}),
			nil,
			&Pair{[]Pattern{&Literal{types.Bool(true)}}, false},
		},
		{
			// (())
			types.NewPair(&types.Pair{}, nil),
			nil,
			&Pair{[]Pattern{&Pair{}}, false},
		},
		{
			// (x (y) z)
			types.PairFromArray([]types.Sexpr{"x", types.NewPair("y", nil), "z"}),
			nil,
			&Pair{[]Pattern{&Identifier{"x", false}, &Pair{[]Pattern{&Identifier{"y", false}}, false}, &Identifier{"z", false}}, false},
		},
		{
			// (x + 1)
			types.PairFromArray([]types.Sexpr{"x", "+", types.Integer(1)}),
			[]types.Symbol{"+"},
			&Pair{[]Pattern{&Identifier{"x", false}, &Literal{"+"}, &Literal{types.Integer(1)}}, false},
		},
		{
			// (x y ...)
			types.PairFromArray([]types.Sexpr{"x", "y", "..."}),
			nil,
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
		},
	}

	for _, tt := range testCases {
		result, err := FromPair(tt.input, tt.literals)
		if err != nil {
			t.Errorf("unexpected error: %s", err)
		}
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, tt.expected, result)
		}
	}
}

func Test_MatchPattern(t *testing.T) {
	var testCases = []struct {
		pattern  Pattern
		input    types.Sexpr
		expected bool
		mapping  mapping.Mapping
	}{
		{
			// literal else
			&Literal{"else"},
			types.Symbol("else"),
			true,
			mapping.Mapping{},
		},
		{
			// literal else
			&Literal{"else"},
			types.Bool(true),
			false,
			mapping.Mapping{},
		},
		{
			// literal else
			&Literal{"else"},
			&types.Pair{},
			false,
			mapping.Mapping{},
		},
		{
			// literal _
			&Literal{"_"},
			types.Symbol("_"),
			true,
			mapping.Mapping{},
		},
		{
			// literal ...
			&Literal{"..."},
			types.Symbol("..."),
			true,
			mapping.Mapping{},
		},
		{
			// x
			&Identifier{"x", false},
			types.Symbol("y"),
			true,
			mapping.Mapping{"x": "y"},
		},
		{
			// x
			&Identifier{"x", false},
			nil,
			false,
			mapping.Mapping{},
		},
		{
			// ()
			&Pair{},
			types.Symbol("x"),
			false,
			mapping.Mapping{},
		},
		{
			// ()
			&Pair{},
			types.Bool(true),
			false,
			mapping.Mapping{},
		},
		{
			// ()
			&Pair{},
			&types.Pair{},
			true,
			mapping.Mapping{},
		},
		{
			// ()
			&Pair{},
			types.NewPair("x", "y"),
			false,
			mapping.Mapping{},
		},
		{
			// (x y)
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", false}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			mapping.Mapping{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b"}},
		},
		{
			// (x y ...)
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.Symbol("b"), types.Symbol("c"), types.Symbol("d")}),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", "c", "d"}},
		},
		{
			// (x y ...)
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.Symbol("b"), &types.Pair{}, types.Bool(false)}),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", &types.Pair{}, types.Bool(false)}},
		},
		{
			// (x y ...)
			&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a"), nil),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{}},
		},
		{
			// (x (y ...) z)
			&Pair{[]Pattern{&Identifier{"x", false}, &Pair{[]Pattern{&Identifier{"y", true}}, false}, &Identifier{"z", false}}, false},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.NewPair(types.Symbol("b"), types.Symbol("c")), types.Symbol("d")}),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{types.Symbol("b"), types.Symbol("c")}, "z": "d"},
		},
		{
			// (x (y ...) z)
			&Pair{[]Pattern{&Identifier{"x", false}, &Pair{[]Pattern{&Identifier{"y", true}}, false}, &Identifier{"z", false}}, false},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), &types.Pair{}, types.Symbol("d")}),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{}, "z": "d"},
		},
		{
			// (x (() (y ...) z) v)
			&Pair{[]Pattern{
				&Identifier{"x", false},
				&Pair{[]Pattern{
					&Pair{},
					&Pair{[]Pattern{&Identifier{"y", true}}, false},
					&Identifier{"z", false},
				}, false},
				&Identifier{"v", false},
			}, false},
			types.PairFromArray([]types.Sexpr{
				types.Symbol("a"),
				types.PairFromArray([]types.Sexpr{
					&types.Pair{},
					types.PairFromArray([]types.Sexpr{"b", "c"}),
					types.Symbol("d")}),
				types.Symbol("e")}),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", "c"}, "z": "d", "v": "e"},
		},
		{
			// (x (() (y ...) z) v)
			&Pair{[]Pattern{
				&Identifier{"x", false},
				&Pair{[]Pattern{
					&Pair{},
					&Pair{[]Pattern{&Identifier{"y", true}}, false},
					&Identifier{"z", false},
				}, false},
				&Identifier{"v", false},
			}, false},
			types.PairFromArray([]types.Sexpr{
				types.Symbol("a"),
				types.PairFromArray([]types.Sexpr{
					&types.Pair{},
					types.Symbol("d"),
				}),
				types.Symbol("e"),
			}),
			false,
			mapping.Mapping{},
		},
		// FIXME
		// {
		// 	// ((x y) ...)
		// 	&Pair{[]Pattern{&Pair{[]Pattern{&Identifier{"x", false}, &Identifier{"y", false}}, true}}, false},
		// 	&types.Pair{},
		// 	true,
		// 	mapping.Mapping{"x": EllipsisVar{}, "y": EllipsisVar{}},
		// },
	}

	for _, tt := range testCases {
		mapping, result := tt.pattern.Match(tt.input)

		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for pattern %v and input %v expected %v, got %v", tt.pattern, tt.input, tt.expected, result)
		}
		if !cmp.Equal(mapping, tt.mapping) {
			t.Errorf("for pattern %v and input %v expected %v, got %v", tt.pattern, tt.input, tt.mapping, mapping)
		}
	}
}
