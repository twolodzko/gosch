package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/types"
)

// (do ((var init update) ...) (test result ...) expr ...)
//
//  1. initialize variables with init steps
//  2. iterate:
//    a. check test, if true eval & return result
//    b. eval exps
//    c. update bindings with update steps
func do(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
	var (
		stop types.Bool
		err  error
	)

	job, err := newDoJob(args, env)
	if err != nil {
		return nil, err
	}

	for {
		// eval clause
		stop, err = job.shouldStop()
		if err != nil {
			return nil, err
		}
		if stop {
			return evalAll(job.clause.Next, job.env)
		}

		// eval body
		_, err = evalAll(job.body, job.env)
		if err != nil {
			return nil, err
		}

		// update bindings
		err = job.updateBindings()
		if err != nil {
			return nil, err
		}
	}
}

type doJob struct {
	steps  map[types.Symbol]types.Sexpr
	clause *types.Pair
	body   *types.Pair
	env    *envir.Env
}

func (job *doJob) shouldStop() (types.Bool, error) {
	test, err := Eval(job.clause.This, job.env)
	return types.IsTrue(test), err
}

func (job *doJob) updateBindings() error {
	newEnv := envir.NewEnv()
	newEnv.Parent = job.env.Parent

	for name, step := range job.steps {
		val, err := Eval(step, job.env)
		if err != nil {
			return err
		}
		newEnv.Set(name, val)
	}

	job.env = newEnv
	return nil
}

func newDoJob(args *types.Pair, env *envir.Env) (doJob, error) {
	if args == nil || !args.HasNext() {
		return doJob{}, errors.New("wrong number of arguments")
	}

	local := envir.NewEnv()
	local.Parent = env

	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return doJob{}, fmt.Errorf("%v is not a list", args.This)
	}
	steps, err := setSteps(bindings, env, local)
	if err != nil {
		return doJob{}, err
	}
	clause := args.Next.This.(*types.Pair)
	if !ok {
		return doJob{}, fmt.Errorf("not a valid clause: %v", args.Next.This)
	}
	body := args.Next.Next

	return doJob{steps, clause, body, local}, nil
}

func setSteps(args *types.Pair, parent, local *envir.Env) (map[types.Symbol]types.Sexpr, error) {
	if args.IsNull() {
		return nil, nil
	}

	steps := make(map[types.Symbol]types.Sexpr)
	head := args
	for head != nil {
		switch pair := head.This.(type) {
		case *types.Pair:
			if pair.IsNull() || !pair.HasNext() {
				return nil, errors.New("wrong number of arguments")
			}

			// variable name
			name, ok := pair.This.(types.Symbol)
			if !ok {
				return nil, fmt.Errorf("%v is not a valid name", pair.This)
			}

			// init variable
			val, err := Eval(pair.Next.This, parent)
			if err != nil {
				return nil, err
			}
			local.Set(name, val)

			// set update step
			if pair.Next.HasNext() {
				steps[name] = pair.Next.Next.This
			} else {
				steps[name] = name
			}
		default:
			return nil, fmt.Errorf("%v is not a list", head.This)
		}
		head = head.Next
	}
	return steps, nil
}

func evalAll(exprs *types.Pair, env *envir.Env) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	head := exprs
	for head != nil {
		result, err = Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		head = head.Next
	}
	return result, nil
}
