package types

import "fmt"

type Symbol string

func IsTrue(s any) bool {
	switch val := s.(type) {
	case bool:
		return val
	default:
		return true
	}
}

func ToString(val any) string {
	switch val := val.(type) {
	case nil:
		return "()"
	case bool:
		if val {
			return "#t"
		} else {
			return "#f"
		}
	case string:
		return fmt.Sprintf("\"%s\"", val)
	default:
		return fmt.Sprintf("%v", val)
	}
}
