package main

import (
	"errors"
	"fmt"
	"strconv"
)

func toInt(obj Any) (int, error) {
	switch x := obj.(type) {
	case int:
		return x, nil
	case string:
		number, err := strconv.Atoi(x)
		if err != nil {
			return 0, err
		}
		return number, nil
	default:
		return 0, fmt.Errorf("%v is not a number", obj)
	}
}

func sum(args *Pair) (Any, error) {
	var result int = 0
	if args.This == nil {
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

func dif(args *Pair) (Any, error) {
	if args.This == nil {
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

func mul(args *Pair) (Any, error) {
	var result int = 1
	if args.This == nil {
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

func div(args *Pair) (Any, error) {
	if args.This == nil {
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

func mod(args *Pair) (Any, error) {
	if args.This == nil || !args.HasNext() {
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

func equal(args *Pair) (Any, error) {
	if args.This == nil || !args.HasNext() {
		return Bool(true), nil
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
			return Bool(false), nil
		}
		prev = curr
	}
	return Bool(true), nil
}

func lower(args *Pair) (Any, error) {
	if args.This == nil || !args.HasNext() {
		return Bool(true), nil
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
			return Bool(false), nil
		}
		prev = curr
	}
	return Bool(true), nil
}

func higher(args *Pair) (Any, error) {
	if args.This == nil || !args.HasNext() {
		return Bool(true), nil
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
			return Bool(false), nil
		}
		prev = curr
	}
	return Bool(true), nil
}
