package main

import "fmt"

func let(args *Pair, env *Env) (Any, error) {
	local := NewEnv()
	local.parent = env

	// bind variables
	bindings, ok := args.This.(*Pair)
	if !ok {
		return nil, fmt.Errorf("%v is not a list", args.This)
	}
	err := local.setBindings(bindings)
	if err != nil {
		return nil, err
	}

	// no body
	if !args.HasNext() {
		return nil, nil
	}
	body := args.Next

	last, local, err := partialEval(body, local)
	if err != nil {
		return nil, err
	}
	return Eval(last, local)
}

// Iterate through the bindings ((name1 value1) (name2 value2) ...) and set them to an environment
func (e *Env) setBindings(bindings *Pair) error {
	head := bindings
	for head != nil {
		binding, ok := head.This.(*Pair)
		if !ok {
			return fmt.Errorf("%v is not a list", head.This)
		}
		// unpack binding (name value)
		if name, ok := binding.This.(string); ok {
			if !binding.HasNext() {
				return fmt.Errorf("%v has not value to bind", binding)
			}
			e.Set(name, binding.Next.This)
		} else {
			return fmt.Errorf("binding %v does not use proper name", binding)
		}
		head = head.Next
	}
	return nil
}

// Evaluate all but last args, return last arg and enclosing environment
func partialEval(args *Pair, env *Env) (Any, *Env, error) {
	current := args
	for current.HasNext() {
		_, err := Eval(current.This, env)
		if err != nil {
			return nil, nil, err
		}
		current = current.Next
	}
	return current.This, env, nil
}

// func lambda(args *Pair, env *Env) (Any, error) {

// }
