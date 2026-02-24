package special_forms

import (
	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/types"
)

// `do` procedure
//
//	(do ((var init update) ...) (test result ...) expr ...)
//
//	1. initialize variables with init steps
//	2. iterate:
//	  a. check test, if true eval & return result
//	  b. eval exps
//	  c. update bindings with update steps
func Do(args any, env *envir.Env) (any, error) {
	var (
		stop bool
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
			var (
				head       = job.clause.Next
				result any = nil
				err    error
			)
			for head != nil {
				p, ok := head.(types.Pair)
				if !ok {
					return nil, eval.ErrSyntax
				}
				result, err = eval.Eval(p.This, job.env)
				head = p.Next
			}
			return result, err
		}

		// eval body
		if job.body != nil {
			err := job.body.TryForEach(func(val any) error {
				_, err := eval.Eval(val, job.env)
				return err
			})
			if err != nil {
				return nil, err
			}
		}

		// update bindings
		err = job.updateBindings()
		if err != nil {
			return nil, err
		}
	}
}

type doJob struct {
	steps  map[types.Symbol]any
	clause types.Pair
	body   *types.Pair
	env    *envir.Env
}

// Evaluate the stopping condition
func (job *doJob) shouldStop() (bool, error) {
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
func newDoJob(args any, env *envir.Env) (doJob, error) {
	local := envir.NewEnvFrom(env)

	//	(do ((var init update) ...) (test result ...) expr ...)

	this, next, ok := unpack(args)
	if !ok {
		return doJob{}, eval.ErrSyntax
	}
	// ((var init update) ...) ...
	steps, err := setSteps(this, env, local)
	if err != nil {
		return doJob{}, err
	}

	// ... (test result ...) ...
	this, next, ok = unpack(next)
	if !ok {
		return doJob{}, eval.ErrSyntax
	}
	clause, ok := this.(types.Pair)
	if !ok {
		return doJob{}, eval.ErrSyntax
	}

	// ... expr ...)
	var body *types.Pair
	if next != nil {
		this, ok := next.(types.Pair)
		if !ok {
			return doJob{}, eval.ErrSyntax
		}
		body = &this
	}

	return doJob{steps, clause, body, local}, nil
}

// Initialize the variable bindings and the update steps
func setSteps(args any, parent, local *envir.Env) (map[types.Symbol]any, error) {
	steps := make(map[types.Symbol]any)
	var (
		ok   bool
		step any
		head = args
	)
	for head != nil {
		step, head, ok = unpack(head)
		if !ok {
			return nil, eval.ErrSyntax
		}

		// variable name
		this, step, ok := unpack(step)
		if !ok {
			return nil, eval.ErrSyntax
		}
		name, ok := this.(types.Symbol)
		if !ok {
			return nil, eval.InvalidName{Val: this}
		}

		// init variable
		expr, step, ok := unpack(step)
		if !ok {
			return nil, eval.ErrSyntax
		}
		val, err := eval.Eval(expr, parent)
		if err != nil {
			return nil, err
		}
		local.Set(name, val)

		// set update step
		if step != nil {
			this, step, ok = unpack(step)
			if !ok || step != nil {
				return nil, eval.ErrSyntax
			}
			steps[name] = this
		} else {
			steps[name] = name
		}
	}
	return steps, nil
}

// If the value is a Pair, extract head and tail, and return success status.
func unpack(val any) (any, any, bool) {
	p, ok := val.(types.Pair)
	return p.This, p.Next, ok
}
