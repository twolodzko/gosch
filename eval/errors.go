package eval

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/types"
)

var ErrBadArgNumber = errors.New("wrong number of arguments")
var ErrInvalidSyntax = errors.New("invalid syntax")

type ErrNonList struct {
	Val types.Sexpr
}

func NewErrNonList(val types.Sexpr) *ErrNonList {
	return &ErrNonList{val}
}

func (e *ErrNonList) Error() string {
	return fmt.Sprintf("%v is not a list", e.Val)
}

type ErrBadName struct {
	Val types.Sexpr
}

func NewErrBadName(val types.Sexpr) *ErrBadName {
	return &ErrBadName{val}
}

func (e *ErrBadName) Error() string {
	return fmt.Sprintf("%v is not a valid name", e.Val)
}
