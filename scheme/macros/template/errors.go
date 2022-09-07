package template

import (
	"errors"
	"fmt"

	"github.com/twolodzko/gosch/types"
)

var ErrEllipsisOutOfBounds = errors.New("ellipsis out of bounds")
var ErrEmptyEllipsis = errors.New("empty ellipsis")
var ErrInvalidEllipsis = errors.New("invalid ellipsis expansion")

type ErrInvalidTemplate struct {
	template types.Sexpr
}

func (e ErrInvalidTemplate) Error() string {
	return fmt.Sprintf("template cannot be expaned: %v", e.template)
}
