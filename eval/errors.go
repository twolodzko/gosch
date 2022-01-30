package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/types"
)

type ErrNonList struct {
	Val types.Sexpr
}

func (e *ErrNonList) Error() string {
	return fmt.Sprintf("%v is not a list", e.Val)
}

type ErrBadName struct {
	Val types.Sexpr
}

func (e *ErrBadName) Error() string {
	return fmt.Sprintf("%v is not a valid name", e.Val)
}

var ErrBadArgNumber = errors.New("wrong number of arguments")
