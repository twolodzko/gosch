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

type Traceback struct {
	caller  types.Sexpr
	message error
}

func NewTraceback(name types.Sexpr, err error) error {
	if err != nil {
		if name == "error" {
			return err
		}
		return Traceback{name, err}
	}
	return nil
}

func (e Traceback) Error() string {
	return fmt.Sprintf("%v : %v", e.caller, e.message)
}
