package macros

import "fmt"

func validatePattern(pattern []Pattern) error {
	if !ellipsisIsLast(pattern) {
		return fmt.Errorf("%s is allowed only at the end of the pattern", Ellipsis)
	}
	keys := duplicatedPatterns{make(map[Pattern]bool)}
	if keys.hasDuplicates(pattern) {
		return fmt.Errorf("duplicated identifiers found in pattern")
	}
	return nil
}

func ellipsisIsLast(pattern []Pattern) bool {
	// FIXME
	// for i, val := range pattern {
	// 	switch val := val.(type) {
	// 	case EllipsisPattern:
	// 		if i != len(pattern)-1 {
	// 			return false
	// 		}
	// 	case *PairPattern:
	// 		if !ellipsisIsLast(val.Patterns) {
	// 			return false
	// 		}
	// 	}
	// }
	return true
}

type duplicatedPatterns struct {
	seen map[Pattern]bool
}

func (d *duplicatedPatterns) hasDuplicates(pattern []Pattern) bool {
	for _, val := range pattern {
		switch val := val.(type) {
		case *IdentifierPattern:
			if _, ok := d.seen[val]; ok {
				return true
			}
			// FIXME: fix for ellipsis
			d.seen[val] = true
		case *PairPattern:
			if d.hasDuplicates(val.Patterns) {
				return true
			}
		}
	}
	return false
}
