package types

import "fmt"

type (
	Sexpr  = interface{}
	Bool   bool
	Symbol = string
	String string
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

func Quote(s Sexpr) Sexpr {
	return &Pair{"quote", &Pair{s, nil}}
}

func (s String) String() string {
	return fmt.Sprintf(`"%v"`, string(s))
}
