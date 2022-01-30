package eval

import (
	"github.com/twolodzko/gosch/types"
)

func equal(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Equal(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}

func lower(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Lower(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}

func greater(args *types.Pair) (types.Sexpr, error) {
	if args == nil || !args.HasNext() {
		return types.Bool(true), nil
	}
	prev := args.This
	head := args
	for head.HasNext() {
		head = head.Next
		switch x := prev.(type) {
		case types.Comparable:
			test, err := x.Greater(head.This)
			if err != nil {
				return nil, err
			}
			if !test {
				return types.Bool(false), nil
			}
		default:
			return nil, &types.ErrNaN{Val: head.This}
		}
		prev = head.This
	}
	return types.Bool(true), nil
}
