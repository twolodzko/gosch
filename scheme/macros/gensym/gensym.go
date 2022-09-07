package gensym

import (
	"fmt"

	"github.com/twolodzko/gosch/types"
)

var Generator gensym

type gensym struct {
	counter uint
}

func (g *gensym) New() types.Symbol {
	g.counter++
	return fmt.Sprintf("g%04d", g.counter)
}

func (g *gensym) Reset() {
	g.counter = 0
}
