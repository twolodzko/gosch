package types

import (
	"fmt"
	"math"
)

type (
	Integer int
	Float   float64
)

type Arithmetic interface {
	Add(Sexpr) (Sexpr, error)
	Sub(Sexpr) (Sexpr, error)
	Mul(Sexpr) (Sexpr, error)
	Div(Sexpr) (Sexpr, error)
	Mod(Sexpr) (Sexpr, error)
}

func (x Integer) Add(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x + y, nil
	case Float:
		return Float(x) + y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Integer) Sub(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x - y, nil
	case Float:
		return Float(x) - y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Integer) Mul(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x * y, nil
	case Float:
		return Float(x) * y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Integer) Div(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return Float(x) / Float(y), nil
	case Float:
		return Float(x) / y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Integer) IntDiv(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x / y, nil
	default:
		return nil, fmt.Errorf("%v is not an integer", y)
	}
}

func (x Integer) Mod(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x % y, nil
	case Float:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Float) Add(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x + Float(y), nil
	case Float:
		return x + y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Float) Sub(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x - Float(y), nil
	case Float:
		return x - y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Float) Mul(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x * Float(y), nil
	case Float:
		return x * y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Float) Div(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		return x / Float(y), nil
	case Float:
		return x / y, nil
	default:
		return nil, &ErrNaN{y}
	}
}

func (x Float) Mod(y Sexpr) (Sexpr, error) {
	switch y := y.(type) {
	case Integer:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	case Float:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	default:
		return nil, &ErrNaN{y}
	}
}

// String representation of float up to two decimal places
func (x Float) String() string {
	num := math.Round(float64(x)*100) / 100
	return fmt.Sprintf("%g", num)
}

type ErrNaN struct {
	Val Sexpr
}

func (e *ErrNaN) Error() string {
	return fmt.Sprintf("%v is not a number", e.Val)
}
