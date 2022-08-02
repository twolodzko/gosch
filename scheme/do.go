package scheme

import (
	"fmt"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `do` procedure
//
//  (do ((var init update) ...) (test result ...) expr ...)
//
//  1. initialize variables with init steps
//  2. iterate:
//    a. check test, if true eval & return result
//    b. eval exps
//    c. update bindings with update steps
func Do(args *types.Pair, env *envir.Env) (types.Sexpr, error) {
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

// Evaluate the stopping condition
func (job *doJob) shouldStop() (types.Bool, error) {
	test, err := eval.Eval(job.clause.This, job.env)
	return types.IsTrue(test), err
}

// Update the variable bindings
func (job *doJob) updateBindings() error {
	newEnv := envir.NewEnvFrom(job.env.Parent)

	for name, step := range job.steps {
		val, err := eval.Eval(step, job.env)
		if err != nil {
			return err
		}
		newEnv.Set(name, val)
	}

	job.env = newEnv
	return nil
}

// Initialize the `do` job
func newDoJob(args *types.Pair, env *envir.Env) (doJob, error) {
	if args == nil || !args.HasNext() {
		return doJob{}, eval.ErrBadArgNumber
	}

	local := envir.NewEnvFrom(env)

	bindings, ok := args.This.(*types.Pair)
	if !ok {
		return doJob{}, eval.NewErrNonList(args.This)
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

// Initialize the variable bindings and the update steps
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
				return nil, eval.ErrBadArgNumber
			}

			// variable name
			name, ok := pair.This.(types.Symbol)
			if !ok {
				return nil, eval.NewErrBadName(pair.This)
			}

			// init variable
			val, err := eval.Eval(pair.Next.This, parent)
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
			return nil, eval.NewErrNonList(head.This)
		}
		head = head.Next
	}
	return steps, nil
}

// Evaluate all expressions, return the last result
func evalAll(exprs *types.Pair, env *envir.Env) (types.Sexpr, error) {
	var (
		result types.Sexpr
		err    error
	)
	head := exprs
	for head != nil {
		result, err = eval.Eval(head.This, env)
		if err != nil {
			return nil, err
		}
		head = head.Next
	}
	return result, nil
}
