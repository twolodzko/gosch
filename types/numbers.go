package types

import (
	"fmt"
	"math"
)

type (
	Integer int
	Float   float64
)

type Arith interface {
	Add(any) (Arith, error)
	Sub(any) (Arith, error)
	Mul(any) (Arith, error)
	Div(any) (Arith, error)
	Mod(any) (Arith, error)
}

func (x Integer) Add(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x + y, nil
	case Float:
		return Float(x) + y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Integer) Sub(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x - y, nil
	case Float:
		return Float(x) - y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Integer) Mul(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x * y, nil
	case Float:
		return Float(x) * y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Integer) Div(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return Float(x) / Float(y), nil
	case Float:
		return Float(x) / y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Integer) IntDiv(y any) (Integer, error) {
	switch y := y.(type) {
	case Integer:
		return Integer(x / y), nil
	default:
		return Integer(0), fmt.Errorf("%v is not an integer", y)
	}
}

func (x Integer) Mod(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x % y, nil
	case Float:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	default:
		return nil, NaN{y}
	}
}

func (x Float) Add(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x + Float(y), nil
	case Float:
		return x + y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Float) Sub(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x - Float(y), nil
	case Float:
		return x - y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Float) Mul(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x * Float(y), nil
	case Float:
		return x * y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Float) Div(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		return x / Float(y), nil
	case Float:
		return x / y, nil
	default:
		return nil, NaN{y}
	}
}

func (x Float) Mod(y any) (Arith, error) {
	switch y := y.(type) {
	case Integer:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	case Float:
		result := math.Mod(float64(x), float64(y))
		return Float(result), nil
	default:
		return nil, NaN{y}
	}
}

// String representation of float up to two decimal places
func (x Float) String() string {
	num := math.Round(float64(x)*100) / 100
	return fmt.Sprintf("%g", num)
}

type NaN struct {
	Val any
}

func (e NaN) Error() string {
	return fmt.Sprintf("%v is not a number", e.Val)
}
