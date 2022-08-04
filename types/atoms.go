package types

import "fmt"

type (
	Sexpr  = interface{}
	Bool   bool
	Symbol = string
	String string
)

const (
	TRUE  = Bool(true)
	FALSE = Bool(false)
)

func (b Bool) String() string {
	if b {
		return "#t"
	}
	return "#f"
}

func IsTrue(s Sexpr) Bool {
	switch val := s.(type) {
	case Bool:
		return val
	default:
		return true
	}
}

func (s String) String() string {
	return fmt.Sprintf(`"%v"`, string(s))
}
