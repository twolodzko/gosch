package main

type (
	Primitive = func(*Pair) (Any, error)
	Procedure = func(*Pair, *Env) (Any, error)
)

func procedure(name string) (Procedure, bool) {
	var fn Primitive
	switch name {
	case "car":
		fn = car
	case "cdr":
		fn = cdr
	case "null?":
		fn = isNull
	case "pair?":
		fn = isPair
	case "cons":
		fn = cons
	case "list":
		fn = list
	case "not":
		fn = not
	case "eq?":
		fn = eq
	case "and":
		fn = and
	case "or":
		fn = or
	case "+":
		fn = sum
	case "-":
		fn = dif
	case "*":
		fn = mul
	case "/":
		fn = div
	case "modulo":
		fn = mod
	case "=":
		fn = equal
	case "<":
		fn = lower
	case ">":
		fn = higher
	case "define":
		return define, true
	case "quote":
		return func(args *Pair, env *Env) (Any, error) {
			return args.This, nil
		}, true
	default:
		return nil, false
	}
	return primitiveWrapper(fn), true
}

func primitiveWrapper(fn Primitive) Procedure {
	return func(args *Pair, env *Env) (Any, error) {
		args, err := evalArgs(args, env)
		if err != nil {
			return nil, err
		}
		return fn(args)
	}
}

func evalArgs(pair *Pair, env *Env) (*Pair, error) {
	var (
		head *Pair
		args []Any
	)
	head = pair
	for head != nil {
		sexpr, err := Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		args = append(args, sexpr)
		head = head.Next
	}
	// TODO: avoid re-packing
	return newPair(args), nil
}
