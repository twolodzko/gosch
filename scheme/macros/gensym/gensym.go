package gensym

import "fmt"

var Generator gensym

type gensym struct {
	counter uint
}

func (g *gensym) New() string {
	g.counter++
	return fmt.Sprintf("g%04d", g.counter)
}
