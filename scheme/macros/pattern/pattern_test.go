package pattern

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

func Test_Extract(t *testing.T) {

	var testCases = []struct {
		input    *types.Pair
		literals []types.Symbol
		expected *Pattern
	}{
		{
			// ()
			&types.Pair{},
			nil,
			&Pattern{&Pair{}},
		},
		{
			// (x)
			types.NewPair("x"),
			nil,
			&Pattern{&Pair{[]Subpattern{&Identifier{"x", false}}, false}},
		},
		{
			// (#t)
			types.NewPair(types.Bool(true)),
			nil,
			&Pattern{&Pair{[]Subpattern{&Literal{types.Bool(true)}}, false}},
		},
		{
			// (x (y) z)
			types.NewPair("x", types.NewPair("y"), "z"),
			nil,
			&Pattern{&Pair{[]Subpattern{&Identifier{"x", false}, &Pair{[]Subpattern{&Identifier{"y", false}}, false}, &Identifier{"z", false}}, false}},
		},
		{
			// (x + 1)
			types.NewPair("x", "+", types.Integer(1)),
			[]types.Symbol{"+"},
			&Pattern{&Pair{[]Subpattern{&Identifier{"x", false}, &Literal{"+"}, &Literal{types.Integer(1)}}, false}},
		},
		{
			// (x y ...)
			types.NewPair("x", "y", "..."),
			nil,
			&Pattern{&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", true}}, false}},
		},
	}

	for _, tt := range testCases {
		result, err := Extract(types.NewPair(tt.input), tt.literals)
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
		pattern     Subpattern
		input       types.Sexpr
		shouldMatch bool
		mapping     mapping.Mapping
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
			&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", false}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			mapping.Mapping{"x": "a", "y": "b"},
		},
		{
			// (x y ...)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b"}},
		},
		{
			// (x y ...)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b"), types.Symbol("c"), types.Symbol("d")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", "c", "d"}},
		},
		{
			// (x y ...)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a"), types.Symbol("b"), &types.Pair{}, types.Bool(false)),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", &types.Pair{}, types.Bool(false)}},
		},
		{
			// (x y ...)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Identifier{"y", true}}, false},
			types.NewPair(types.Symbol("a")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{}},
		},
		{
			// (x (y ...) z)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Pair{[]Subpattern{&Identifier{"y", true}}, false}, &Identifier{"z", false}}, false},
			types.NewPair(types.Symbol("a"), types.NewPair(types.Symbol("b"), types.Symbol("c")), types.Symbol("d")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{types.Symbol("b"), types.Symbol("c")}, "z": "d"},
		},
		{
			// (x (y ...) z)
			&Pair{[]Subpattern{&Identifier{"x", false}, &Pair{[]Subpattern{&Identifier{"y", true}}, false}, &Identifier{"z", false}}, false},
			types.NewPair(types.Symbol("a"), &types.Pair{}, types.Symbol("d")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{}, "z": "d"},
		},
		{
			// (x (() (y ...) z) v)
			&Pair{[]Subpattern{
				&Identifier{"x", false},
				&Pair{[]Subpattern{
					&Pair{},
					&Pair{[]Subpattern{&Identifier{"y", true}}, false},
					&Identifier{"z", false},
				}, false},
				&Identifier{"v", false},
			}, false},
			types.NewPair(
				types.Symbol("a"),
				types.NewPair(
					&types.Pair{},
					types.NewPair("b", "c"),
					types.Symbol("d")),
				types.Symbol("e")),
			true,
			mapping.Mapping{"x": "a", "y": EllipsisVar{"b", "c"}, "z": "d", "v": "e"},
		},
		{
			// (x (() (y ...) z) v)
			&Pair{[]Subpattern{
				&Identifier{"x", false},
				&Pair{[]Subpattern{
					&Pair{},
					&Pair{[]Subpattern{&Identifier{"y", true}}, false},
					&Identifier{"z", false},
				}, false},
				&Identifier{"v", false},
			}, false},
			types.NewPair(
				types.Symbol("a"),
				types.NewPair(
					&types.Pair{},
					types.Symbol("d"),
				),
				types.Symbol("e"),
			),
			false,
			mapping.Mapping{},
		},
		{
			// (x (y (z ...) ...) ...)
			&Pair{[]Subpattern{
				&Identifier{"x", false},
				&Pair{[]Subpattern{
					&Identifier{"y", false},
					&Pair{[]Subpattern{
						&Identifier{"z", true},
					}, true},
				}, true},
			}, false},
			types.NewPair("a"),
			true,
			mapping.Mapping{"x": "a"},
		},
		{
			// (x (y (z ...) ...) ...)
			&Pair{[]Subpattern{
				&Identifier{"x", false},
				&Pair{[]Subpattern{
					&Identifier{"y", false},
					&Pair{[]Subpattern{
						&Identifier{"z", true},
					}, true},
				}, true},
			}, false},
			types.NewPair("a"),
			true,
			mapping.Mapping{"x": "a"},
		},
		{
			// (x (y (z ...) ...) ...)
			&Pair{[]Subpattern{
				&Identifier{"x", false},
				&Pair{[]Subpattern{
					&Identifier{"y", false},
					&Pair{[]Subpattern{
						&Identifier{"z", true},
					}, true},
				}, true},
			}, false},
			// (a (b) (c (d e) (f)) (g (h)))
			types.NewPair(
				"a",
				types.NewPair("b"),
				types.NewPair("c", types.NewPair("d", "e"), types.NewPair("f")),
				types.NewPair("g", types.NewPair("h")),
			),
			true,
			mapping.Mapping{
				"x": "a",
				"y": EllipsisVar{"b", "c", "g"},
				"z": EllipsisVar{
					EllipsisVar{EllipsisVar{"d", "e"}, EllipsisVar{"f"}},
					EllipsisVar{EllipsisVar{"h"}},
				},
			},
		},
	}

	for _, tt := range testCases {
		mapping, ok := tt.pattern.Match(tt.input)

		if ok != tt.shouldMatch {
			t.Errorf("for pattern %v and input %v expected %v, got %v", tt.pattern, tt.input, tt.shouldMatch, ok)
			return
		}
		if !cmp.Equal(mapping, tt.mapping) {
			t.Errorf("for pattern %v and input %v expected %v, got %v", tt.pattern, tt.input, tt.mapping, mapping)
		}
	}
}
