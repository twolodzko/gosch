package pattern

import (
	"fmt"
	"reflect"

	"github.com/twolodzko/gosch/scheme/macros/mapping"
	"github.com/twolodzko/gosch/types"
)

var _ Subpattern = (*Identifier)(nil)
var _ Subpattern = (*Literal)(nil)

type Identifier struct {
	Name     types.Symbol
	Repeated bool
}

func (p Identifier) Match(obj types.Sexpr) (mapping.Mapping, bool) {
	if obj == nil {
		return mapping.Mapping{}, false
	}
	return mapping.Mapping{p.Name: obj}, true
}

func (p Identifier) String() string {
	if p.Repeated {
		return fmt.Sprintf("%s ...", p.Name)
	}
	return p.Name
}

func (p *Identifier) ToEllipsis() {
	p.Repeated = true
}

func (p Identifier) IsEllipsis() bool {
	return p.Repeated
}

type Literal struct {
	Value types.Sexpr
}

func (p Literal) Match(obj types.Sexpr) (mapping.Mapping, bool) {
	return mapping.Mapping{}, reflect.DeepEqual(obj, p.Value)
}

func (p Literal) String() string {
	return fmt.Sprintf("%s", p.Value)
}

func (p *Literal) ToEllipsis() {
	// cannot be an ellipsis
}

func (p Literal) IsEllipsis() bool {
	return false
}
