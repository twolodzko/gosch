package extended_types

import (
	"fmt"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `vector` procedure
func Vector(args *types.Pair) (types.Sexpr, error) {
	var vec types.Vector
	head := args
	for head != nil {
		vec = append(vec, head.This)
		head = head.Next
	}
	return &vec, nil
}

// `vector?` procedure
func IsVector(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return nil, eval.ErrBadArgNumber
	}
	switch args.This.(type) {
	case *types.Vector:
		return types.TRUE, nil
	default:
		return types.FALSE, nil
	}
}

// `vector-length` procedure
func VectorLength(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() || args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}
	switch vec := args.This.(type) {
	case *types.Vector:
		return types.Integer(len(*vec)), nil
	default:
		return nil, fmt.Errorf("%s is not a vector", args.This)
	}
}

// `make-vector` procedure
func MakeVector(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() {
		return nil, eval.ErrBadArgNumber
	}

	var vec types.Vector
	switch n := args.This.(type) {
	case types.Integer:
		vec = types.NewVector(int(n))
	default:
		return nil, fmt.Errorf("%s is not a vector", args.This)
	}

	if args.HasNext() {
		if args.Next.HasNext() {
			return nil, eval.ErrBadArgNumber
		}
		for i := range vec {
			vec[i] = args.Next.This
		}
	}

	return &vec, nil
}

// `vector-ref` procedure
func VectorRef(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() || !args.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	vec, ok := args.This.(*types.Vector)
	if !ok {
		return nil, fmt.Errorf("%s is not a vector", args.This)
	}

	switch pos := args.Next.This.(type) {
	case types.Integer:
		if int(pos) >= len(*vec) {
			return nil, fmt.Errorf("%v is not in the correct range", pos)
		}
		return (*vec)[int(pos)], nil
	default:
		return nil, fmt.Errorf("%s is not an integer", args.Next.This)
	}
}

// `vector-set!` procedure
func VectorSet(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() || !args.HasNext() || !args.Next.HasNext() {
		return nil, eval.ErrBadArgNumber
	}

	vec, ok := args.This.(*types.Vector)
	if !ok {
		return nil, fmt.Errorf("%s is not a vector", args.This)
	}

	var pos int
	switch obj := args.Next.This.(type) {
	case types.Integer:
		pos = int(obj)
		if pos >= len(*vec) {
			return nil, fmt.Errorf("%v is not in the correct range", pos)
		}
	default:
		return nil, fmt.Errorf("%s is not an integer", args.Next.This)
	}

	(*vec)[pos] = args.Next.Next.This
	return vec, nil
}

// `vector->list` procedure
func VectorToList(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() {
		return nil, eval.ErrBadArgNumber
	}

	switch vec := args.This.(type) {
	case *types.Vector:
		ap := types.NewAppendablePair()
		for _, x := range *vec {
			ap.Append(x)
		}
		return ap.ToPair(), nil
	default:
		return nil, fmt.Errorf("%s is not a vectpr", args.This)
	}
}

// `list->vector` procedure
func ListToVector(args *types.Pair) (types.Sexpr, error) {
	if args == nil || args.IsNull() {
		return nil, eval.ErrBadArgNumber
	}

	switch pair := args.This.(type) {
	case *types.Pair:
		var vec types.Vector
		head := pair
		for head != nil && !head.IsNull() {
			vec = append(vec, head.This)
			head = head.Next
		}
		return vec, nil
	default:
		return nil, fmt.Errorf("%s is not a vectpr", args.This)
	}
}
