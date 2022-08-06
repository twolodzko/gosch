package macros

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/types"
)

func Test_ExtractPattern(t *testing.T) {
	self := types.Symbol("foobar")

	var testCases = []struct {
		input    *types.Pair
		literals []types.Symbol
		expected []Pattern
	}{
		{
			// (_)
			types.PairFromArray([]types.Sexpr{Self}),
			nil,
			[]Pattern{SelfPattern{self}},
		},
		{
			// (_ x)
			types.PairFromArray([]types.Sexpr{Self, "x"}),
			nil,
			[]Pattern{SelfPattern{self}, IdentifierPattern{"x"}},
		},
		{
			// (_ #t)
			types.PairFromArray([]types.Sexpr{Self, types.Bool(true)}),
			nil,
			[]Pattern{SelfPattern{self}, LiteralPattern{types.Bool(true)}},
		},
		// FIXME
		// {
		// 	// (_ ())
		// 	types.NewPair(self, &types.Pair{}),
		// 	nil,
		// 	[]Pattern{SelfPattern{self}, PairPattern{}},
		// },
		{
			// (_ x (y) z)
			types.PairFromArray([]types.Sexpr{Self, "x", types.NewPair("y", nil), "z"}),
			nil,
			[]Pattern{SelfPattern{self}, IdentifierPattern{"x"}, PairPattern{[]Pattern{IdentifierPattern{"y"}}}, IdentifierPattern{"z"}},
		},
		{
			// (_ x + 1)
			types.PairFromArray([]types.Sexpr{Self, "x", "+", types.Integer(1)}),
			[]types.Symbol{"+"},
			[]Pattern{SelfPattern{self}, IdentifierPattern{"x"}, LiteralPattern{"+"}, LiteralPattern{types.Integer(1)}},
		},
		{
			// (_ x + 1)
			types.PairFromArray([]types.Sexpr{Self, "x", "+", types.Integer(1)}),
			[]types.Symbol{"+"},
			[]Pattern{SelfPattern{self}, IdentifierPattern{"x"}, LiteralPattern{"+"}, LiteralPattern{types.Integer(1)}},
		},
		{
			// (_ x y ...)
			types.PairFromArray([]types.Sexpr{Self, "x", "y", "..."}),
			nil,
			[]Pattern{SelfPattern{self}, IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}},
		},
	}

	for _, tt := range testCases {
		result, err := ExtractPattern(tt.input, self, tt.literals)
		if err != nil {
			t.Errorf("unexpected error: %v", err)
		}
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
		mapping  Mapping
	}{
		{
			// literal else
			LiteralPattern{"else"},
			types.Symbol("else"),
			true,
			Mapping{},
		},
		{
			// literal else
			LiteralPattern{"else"},
			types.Bool(true),
			false,
			Mapping{},
		},
		{
			// literal else
			LiteralPattern{"else"},
			&types.Pair{},
			false,
			Mapping{},
		},
		{
			// literal _
			LiteralPattern{"_"},
			types.Symbol("_"),
			true,
			Mapping{},
		},
		{
			// literal ...
			LiteralPattern{"..."},
			types.Symbol("..."),
			true,
			Mapping{},
		},
		{
			// _
			SelfPattern{"foo"},
			types.Symbol("_"),
			true,
			Mapping{},
		},
		{
			// _
			SelfPattern{"foo"},
			types.Symbol("x"),
			false,
			Mapping{},
		},
		{
			// _
			SelfPattern{"foo"},
			types.Symbol("..."),
			false,
			Mapping{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("..."),
			true,
			Mapping{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("_"),
			false,
			Mapping{},
		},
		{
			// ...
			EllipsisPattern{},
			types.Symbol("x"),
			false,
			Mapping{},
		},
		{
			// x
			IdentifierPattern{"x"},
			types.Symbol("y"),
			true,
			Mapping{"x": "y"},
		},
		{
			// x
			IdentifierPattern{"x"},
			nil,
			false,
			Mapping{},
		},
		{
			// ()
			PairPattern{},
			types.Symbol("x"),
			false,
			Mapping{},
		},
		{
			// ()
			PairPattern{},
			types.Bool(true),
			false,
			Mapping{},
		},
		{
			// ()
			PairPattern{},
			&types.Pair{},
			true,
			Mapping{},
		},
		{
			// (_ x)
			PairPattern{[]Pattern{SelfPattern{"foo"}, IdentifierPattern{"x"}}},
			&types.Pair{},
			false,
			Mapping{},
		},
		{
			// ()
			PairPattern{},
			types.NewPair("x", "y"),
			false,
			Mapping{},
		},
		{
			// (x y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}}},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			Mapping{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			Mapping{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.Symbol("b"), types.Symbol("c"), types.Symbol("d")}),
			true,
			Mapping{"x": "a", "y": "b", "...": types.PairFromArray([]types.Sexpr{types.Symbol("c"), types.Symbol("d")})},
		},
		{
			// (x y ...)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, IdentifierPattern{"y"}, EllipsisPattern{}}},
			types.NewPair(types.Symbol("a"), nil),
			false,
			Mapping{},
		},
		{
			// (x (...) y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, PairPattern{[]Pattern{EllipsisPattern{}}}, IdentifierPattern{"y"}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), types.NewPair(types.Symbol("b"), types.Symbol("c")), types.Symbol("d")}),
			true,
			Mapping{"x": "a", "...": types.PairFromArray([]types.Sexpr{types.Symbol("b"), types.Symbol("c")}), "y": "d"},
		},
		{
			// (x (...) y)
			PairPattern{[]Pattern{IdentifierPattern{"x"}, PairPattern{[]Pattern{EllipsisPattern{}}}, IdentifierPattern{"y"}}},
			types.PairFromArray([]types.Sexpr{types.Symbol("a"), &types.Pair{}, types.Symbol("d")}),
			true,
			Mapping{"x": "a", "y": "d"},
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
			Mapping{"x": "a", "y": "d", "z": "e", "...": types.PairFromArray([]types.Sexpr{"b", "c"})},
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
			Mapping{},
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
