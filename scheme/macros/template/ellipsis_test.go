package template

import (
	"testing"

	"github.com/google/go-cmp/cmp"
	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/scheme/macros/pattern"
)

func Test_extractEllipsis(t *testing.T) {

	m := mapping.Mapping{
		"x": 1,
		"y": pattern.EllipsisVar{1, 2, 3},
		"z": pattern.NestedEllipsis{pattern.EllipsisVar{1, 2, 3, 4}, pattern.EllipsisVar{5, 6}},
	}

	var testCases = []struct {
		index    int
		expected mapping.Mapping
	}{
		{
			0,
			mapping.Mapping{
				"x": 1,
				"y": pattern.EllipsisVar{1, 2, 3},
				"z": pattern.EllipsisVar{1, 2, 3, 4},
			},
		},
		{
			1,
			mapping.Mapping{
				"x": 1,
				"y": pattern.EllipsisVar{1, 2, 3},
				"z": pattern.EllipsisVar{5, 6},
			},
		},
	}

	for _, tt := range testCases {
		result, _ := extractEllipsis(tt.index, m)
		if !cmp.Equal(result, tt.expected) {
			t.Errorf("for index %d we expected %v, got %v", tt.index, tt.expected, result)
		}
	}
}
