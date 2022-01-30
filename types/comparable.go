package types

type Comparable interface {
	Equal(Sexpr) (Bool, error)
	Lower(Sexpr) (Bool, error)
	Greater(Sexpr) (Bool, error)
}

func (x Integer) Equal(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x == y, nil
	case Float:
		return Float(x) == y, nil
	default:
		return false, &ErrNaN{y}
	}
}

func (x Integer) Lower(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x < y, nil
	case Float:
		return Float(x) < y, nil
	default:
		return false, &ErrNaN{y}
	}
}

func (x Integer) Greater(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x > y, nil
	case Float:
		return Float(x) > y, nil
	default:
		return false, &ErrNaN{y}
	}
}

func (x Float) Equal(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x == Float(y), nil
	case Float:
		return x == y, nil
	default:
		return false, &ErrNaN{y}
	}
}

func (x Float) Lower(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x < Float(y), nil
	case Float:
		return x < y, nil
	default:
		return false, &ErrNaN{y}
	}
}

func (x Float) Greater(y Sexpr) (Bool, error) {
	switch y := y.(type) {
	case Integer:
		return x > Float(y), nil
	case Float:
		return x > y, nil
	default:
		return false, &ErrNaN{y}
	}
}
