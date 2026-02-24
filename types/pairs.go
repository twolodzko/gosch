package types

import (
	"fmt"
	"strings"
)

type Pair struct {
	This any
	Next any
}

func List(elems ...any) any {
	var tail any = nil
	for i := len(elems) - 1; i >= 0; i-- {
		tail = Pair{elems[i], tail}
	}
	return tail
}

func Cons(elems ...any) Pair {
	last := len(elems) - 1
	prev := elems[last]
	var pair Pair
	for i := last - 1; i >= 0; i-- {
		pair = Pair{elems[i], prev}
		prev = pair
	}
	return pair
}

func (p Pair) Len() int {
	var len int
	var head any = p
	for {
		switch p := head.(type) {
		case Pair:
			head = p.Next
		case nil:
			return len
		default:
			return len + 1
		}
		len++
	}
}

func (p Pair) String() string {
	if s, ok := p.This.(Symbol); ok {
		switch s {
		case "quote":
			s := ToString(p.Next.(Pair).This)
			return fmt.Sprintf("'%s", s)
		case "quasiquote":
			s := ToString(p.Next.(Pair).This)
			return fmt.Sprintf("`%s", s)
		case "unquote":
			s := ToString(p.Next.(Pair).This)
			return fmt.Sprintf(",%s", s)
		}
	}
	return fmt.Sprintf("(%v)", p.ToString())
}

func (p Pair) ToString() string {
	var (
		acc  []string
		head any = p
	)
	for head != nil {
		switch p := head.(type) {
		case Pair:
			acc = append(acc, ToString(p.This))
			head = p.Next
		default:
			return fmt.Sprintf("%v . %v", strings.Join(acc, " "), head)
		}
	}
	return strings.Join(acc, " ")
}

// For at least one value in the Pair, the function is true
func (p Pair) Any(fn func(val any) bool) bool {
	var head any = p
	for head != nil {
		switch p := head.(type) {
		case Pair:
			if fn(p.This) {
				return true
			}
			head = p.Next
		default:
			return fn(head)
		}
	}
	return false
}

// Map the function to all the elements of the Pair, return modified Pair
func (p Pair) Map(fn func(val any) any) []any {
	var (
		acc  []any
		head any = p
	)
	for {
		switch p := head.(type) {
		case Pair:
			acc = append(acc, fn(p.This))
			head = p.Next
		case nil:
			acc = append(acc, nil)
			return acc
		default:
			acc = append(acc, fn(head))
			return acc
		}
	}
}

// Map the function to all the elements of the Pair, return modified Pair
func (p Pair) TryMap(fn func(val any) (any, error)) ([]any, error) {
	var (
		acc  []any
		head any = p
	)
	for {
		switch p := head.(type) {
		case Pair:
			v, err := fn(p.This)
			if err != nil {
				return nil, err
			}
			acc = append(acc, v)
			head = p.Next
		case nil:
			acc = append(acc, nil)
			return acc, nil
		default:
			v, err := fn(head)
			acc = append(acc, v)
			return acc, err
		}
	}
}

// Map the function to all the elements of the Pair, return modified Pair
func (p Pair) TryForEach(fn func(val any) error) error {
	var head any = p
	for head != nil {
		switch p := head.(type) {
		case Pair:
			err := fn(p.This)
			if err != nil {
				return err
			}
			head = p.Next
		default:
			return fn(head)
		}
	}
	return nil
}
