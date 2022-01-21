package main

import (
	"errors"
	"fmt"
	"strings"
)

type Lambda struct {
	Vars []string
	Body *Pair
}

func newLambda(args *Pair, env *Env) (Any, error) {
	var vars []string
	switch pair := args.This.(type) {
	case *Pair:
		head := pair
		for head != nil {
			if name, ok := head.This.(string); ok {
				vars = append(vars, name)
			} else {
				return Lambda{}, fmt.Errorf("%v is not a valid argument name", pair.This)
			}
			head = head.Next
		}
	default:
		return Lambda{}, fmt.Errorf("%v is not a list", args.This)
	}
	return Lambda{vars, args.Next}, nil
}

func (l Lambda) Call(args *Pair, env *Env) (Any, *Env, error) {
	local := NewEnv()
	local.parent = env

	head := args
	for _, name := range l.Vars {
		if head == nil {
			return nil, env, errors.New("wrong number of arguments")
		}
		local.Set(name, head.This)
		head = head.Next
	}
	return partialEval(l.Body, local)
}

func (l Lambda) String() string {
	vars := strings.Join(l.Vars, " ")
	body := ""
	head := l.Body
	for head != nil {
		body += fmt.Sprintf("%v", head.This)
		if head.HasNext() {
			body += " "
		}
		head = head.Next
	}
	return fmt.Sprintf("(lambda (%v) %v)", vars, body)
}
