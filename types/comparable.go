package types

type Comparable interface {
	Equal(any) (bool, error)
	Lower(any) (bool, error)
	Greater(any) (bool, error)
}

func (x Integer) Equal(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x == y, nil
	case Float:
		return Float(x) == y, nil
	default:
		return false, NaN{y}
	}
}

func (x Integer) Lower(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x < y, nil
	case Float:
		return Float(x) < y, nil
	default:
		return false, NaN{y}
	}
}

func (x Integer) Greater(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x > y, nil
	case Float:
		return Float(x) > y, nil
	default:
		return false, NaN{y}
	}
}

func (x Float) Equal(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x == Float(y), nil
	case Float:
		return x == y, nil
	default:
		return false, NaN{y}
	}
}

func (x Float) Lower(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x < Float(y), nil
	case Float:
		return x < y, nil
	default:
		return false, NaN{y}
	}
}

func (x Float) Greater(y any) (bool, error) {
	switch y := y.(type) {
	case Integer:
		return x > Float(y), nil
	case Float:
		return x > y, nil
	default:
		return false, NaN{y}
	}
}
