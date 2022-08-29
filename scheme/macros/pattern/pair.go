package pattern

import (
	"fmt"
	"strings"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

var _ Subpattern = (*Literal)(nil)

type Pair struct {
	Patterns []Subpattern
	Repeated bool
}

func (p Pair) Match(obj types.Sexpr) (mapping.Mapping, bool) {
	pair, ok := obj.(*types.Pair)
	if !ok {
		return mapping.Mapping{}, false
	}
	if len(p.Patterns) == 0 {
		return mapping.Mapping{}, pair == nil || bool(pair.IsNull())
	}
	return matchAll(p.Patterns, pair)
}

func (p Pair) String() string {
	var str []string
	for _, v := range p.Patterns {
		str = append(str, fmt.Sprintf("%s", v))
	}
	if p.Repeated {
		return fmt.Sprintf("(%s) ...", strings.Join(str, " "))
	}
	return fmt.Sprintf("(%s)", strings.Join(str, " "))
}

func (p *Pair) ToEllipsis() {
	p.Repeated = true
}

func (p Pair) IsEllipsis() bool {
	return p.Repeated
}
