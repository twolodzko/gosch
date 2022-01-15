package main

type Sexpr struct {
	value interface{}
}

type List struct {
	this Sexpr
	next *List
}

func Parse(s string) Sexpr {
	if len(s) > 0 {
		return Sexpr{s}
	}
	return Sexpr{}
}
