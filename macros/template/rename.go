package template

import (
	"fmt"
	"math/big"
	"time"

	"github.com/twolodzko/gosch/types"
)

func Rename(name types.Symbol) types.Symbol {
	suffix := big.NewInt(time.Now().UnixNano()).Text(62)
	return fmt.Sprintf("<%s:%s>", name, suffix)
}
