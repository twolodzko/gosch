package macros

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func Test_PairFromArray(t *testing.T) {

	var testCases = []struct {
		input    *types.Pair
		literals []types.Symbol
		expected []Pattern
	}{
		{
			// ()
			types.PairFromArray([]types.Sexpr{}),
			nil,
			nil,
		},
		{
			// (x)
			types.PairFromArray([]types.Sexpr{"x"}),
			nil,
			[]Pattern{IdentifierPattern{"x"}},
		},
		{
			// (#t)
			types.PairFromArray([]types.Sexpr{types.Bool(true)}),
			nil,
			[]Pattern{LiteralPattern{types.Bool(true)}},
		},
		{
			// (())
			types.NewPair(&types.Pair{}, nil),
			nil,
			[]Pattern{PairPattern{}},
		},
		{
			// (x (y) z)
			types.PairFromArray([]types.Sexpr{"x", types.NewPair("y", nil), "z"}),
			nil,
			[]Pattern{IdentifierPattern{"x"}, PairPattern{[]Pattern{IdentifierPattern{"y"}}}, IdentifierPattern{"z"}},
		},
		{
			// (x + 1)
			types.PairFromArray([]types.Sexpr{"x", "+", types.Integer(1)}),
			[]types.Symbol{"+"},
			[]Pattern{IdentifierPattern{"x"}, LiteralPattern{"+"}, LiteralPattern{types.Integer(1)}},
		},
		{
			// (x y ...)
			types.PairFromArray([]types.Sexpr{"x", "y", "..."}),
			nil,
			[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}},
		},
	}

	for _, tt := range testCases {
		result := patternFromPair(tt.input, tt.literals)
		expected := PairPattern{tt.expected}
		if !cmp.Equal(result, expected) {
			t.Errorf("for %v expected %v, got %v", tt.input, expected, result)
		}
	}
}

func Test_MatchPattern(t *testing.T) {
	var testCases = []struct {
		pattern  Pattern
		input    types.Sexpr
		expected bool
		mapping  Mappings
	}{
		{
			// literal else
			LiteralPattern{"else"},
			types.Symbol("else"),
			true,
			Mappings{},
		},
		{
			// literal else
			LiteralPattern{"else"},
			types.Bool(true),
			false,
			Mappings{},
		},
		{
			// literal else
			LiteralPattern{"else"},
			&types.Pair{},
			false,
			Mappings{},
		},
		{
			// literal _
			LiteralPattern{"_"},
			types.Symbol("_"),
			true,
			Mappings{},
		},
		{
			// literal ...
			LiteralPattern{"..."},
			types.Symbol("..."),
			true,
			Mappings{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("..."),
			true,
			Mappings{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("_"),
			false,
			Mappings{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("x"),
			false,
			Mappings{},
		},
		{
			// x
			IdentifierPattern{"x"},
			types.Symbol("y"),
			true,
			Mappings{"x": "y"},
		},
		{
			// x
			IdentifierPattern{"x"},
			nil,
			false,
			Mappings{},
		},
		{
			// ()
			PairPattern{},
			types.Symbol("x"),
			false,
			Mappings{},
		},
		{
			// ()
			PairPattern{},
			types.Bool(true),
			false,
			Mappings{},
		},
		{
			// ()
			PairPattern{},
			&types.Pair{},
			true,
			Mappings{},
		},
		{
			// ()
			PairPattern{},
			types.NewPair("x", "y"),
			false,
			Mappings{},
		},
		{
			// (x y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}}},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			Mappings{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			Mappings{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.Symbol("b"), types.Symbol("c"), types.Symbol("d")}),
			true,
			Mappings{"x": "a", "y": "b", "...": types.PairFromArray([]types.Sexpr{"c", "d"})},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.Symbol("b"), &types.Pair{}, types.Bool(false)}),
			true,
			Mappings{"x": "a", "y": "b", "...": types.PairFromArray([]types.Sexpr{&types.Pair{}, types.Bool(false)})},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.NewPair(types.Symbol("a"), nil),
			false,
			Mappings{},
		},
		{
			// (x (...) y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, PairPattern{[]Pattern{EllipsisPattern{}}}, IdentifierPattern{"y"}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.NewPair(types.Symbol("b"), types.Symbol("c")), types.Symbol("d")}),
			true,
			Mappings{"x": "a", "...": types.PairFromArray([]types.Sexpr{types.Symbol("b"), types.Symbol("c")}), "y": "d"},
		},
		{
			// (x (...) y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, PairPattern{[]Pattern{EllipsisPattern{}}}, IdentifierPattern{"y"}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), &types.Pair{}, types.Symbol("d")}),
			true,
			Mappings{"x": "a", "y": "d"},
		},
		{
			// (x ((() (...) y) z)
			PairPattern{[]Pattern{
				IdentifierPattern{"x"},
				PairPattern{[]Pattern{
					PairPattern{},
					PairPattern{[]Pattern{EllipsisPattern{}}},
					IdentifierPattern{"y"}}},
				IdentifierPattern{"z"}}},
			types.PairFromArray([]types.Sexpr{
				types.Symbol("a"),
				types.PairFromArray([]types.Sexpr{
					&types.Pair{},
					types.PairFromArray([]types.Sexpr{"b", "c"}),
					types.Symbol("d")}),
				types.Symbol("e")}),
			true,
			Mappings{"x": "a", "y": "d", "z": "e", "...": types.PairFromArray([]types.Sexpr{"b", "c"})},
		},
		{
			// (x ((() (...) y) z)
			PairPattern{[]Pattern{
				IdentifierPattern{"x"},
				PairPattern{[]Pattern{
					PairPattern{},
					PairPattern{[]Pattern{EllipsisPattern{}}},
					IdentifierPattern{"y"}}},
				IdentifierPattern{"z"}}},
			types.PairFromArray([]types.Sexpr{
				types.Symbol("a"),
				types.PairFromArray([]types.Sexpr{
					&types.Pair{},
					types.Symbol("d")}),
				types.Symbol("e")}),
			false,
			Mappings{},
		},
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
