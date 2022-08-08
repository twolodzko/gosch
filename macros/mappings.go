package macros

import (
	"github.com/twolodzko/gosch/types"
)

type Mapping map[types.Symbol]types.Sexpr

// func (m Mapping) SetEnv(env *envir.Env) error {
// 	for key, val := range m {
// 		switch val := val.(type) {
// 		case EllipsisVars:
// 			err := val.SetEnv(env)
// 			if err != nil {
// 				return err
// 			}
// 		default:
// 			val, err := eval.Eval(val, env)
// 			if err != nil {
// 				return err
// 			}
// 			env.Set(key, val)
// 		}
// 	}
// 	return nil
// }

func mergeMappings(x, y Mapping) (Mapping, bool) {
	for key, val := range y {
		if _, ok := x[key]; ok {
			return Mapping{}, false
		}
		x[key] = val
	}
	return x, true
}

type EllipsisVars []types.Sexpr

func ToEllypsisVars(ellipsis types.Sexpr) EllipsisVars {
	var vars EllipsisVars
	switch obj := ellipsis.(type) {
	case *types.Pair:
		i := 1
		head := obj
		for head != nil && !head.IsNull() {
			vars = append(vars, head.This)
			i++
			head = head.Next
		}
	default:
		vars = append(vars, obj)
	}
	return vars
}

// func (v EllipsisVars) SetEnv(env *envir.Env) error {
// 	for i, val := range v {
// 		key := ellipsisName(i + 1)
// 		val, err := eval.Eval(val, env)
// 		if err != nil {
// 			return err
// 		}
// 		env.Set(key, val)
// 	}
// 	return nil
// }

// func ellipsisName(i int) string {
// 	return fmt.Sprintf("%s%d", Ellipsis, i)
// }
