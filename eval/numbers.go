package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/types"
)

func toInt(s types.Sexpr) (int, error) {
	switch x := s.(type) {
	case int:
		return x, nil
	default:
		return 0, fmt.Errorf("%v is not a number", s)
	}
}

func sum(args *types.Pair) (types.Sexpr, error) {
	var result int = 0
	if args == nil {
		return result, nil
	}
	head := args
	for head != nil {
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result += x
		head = head.Next
	}
	return result, nil
}

func dif(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return 0, nil
	}
	result, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	if !args.HasNext() {
		return -result, nil
	}
	head := args
	for head.HasNext() {
		head = head.Next
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result -= x
	}
	return result, nil
}

func mul(args *types.Pair) (types.Sexpr, error) {
	var result int = 1
	if args == nil {
		return result, nil
	}
	head := args
	for head != nil {
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result *= x
		head = head.Next
	}
	return result, nil
}

func div(args *types.Pair) (types.Sexpr, error) {
	if args == nil {
		return 1, nil
	}
	result, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	if !args.HasNext() {
		return 1 / result, nil
	}
	head := args
	for head.HasNext() {
		head = head.Next
		x, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		result /= x
	}
	return result, nil
}

func mod(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return nil, errors.New("wrong number of arguments")
	}
	a, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	n, err := toInt(args.Next.This)
	if err != nil {
		return nil, err
	}
	return a % n, nil
}

func equal(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	head := args
	for head.HasNext() {
		head = head.Next
		curr, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		if prev != curr {
			return types.Bool(false), nil
		}
		prev = curr
	}
	return types.Bool(true), nil
}

func lower(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	head := args
	for head.HasNext() {
		head = head.Next
		curr, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		if prev >= curr {
			return types.Bool(false), nil
		}
		prev = curr
	}
	return types.Bool(true), nil
}

func higher(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev, err := toInt(args.This)
	if err != nil {
		return nil, err
	}
	head := args
	for head.HasNext() {
		head = head.Next
		curr, err := toInt(head.This)
		if err != nil {
			return nil, err
		}
		if prev <= curr {
			return types.Bool(false), nil
		}
		prev = curr
	}
	return types.Bool(true), nil
}
