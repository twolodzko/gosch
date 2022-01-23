package main

import (
	"fmt"
	"io"
	"os"

	"github.com/twolodzko/gosch/repl"
)

const prompt string = "> "

func main() {
	repl := repl.NewRepl(os.Stdin)

	fmt.Println("Press ^C to exit.")
	fmt.Println()

	for {
		fmt.Printf("%s", prompt)

		objs, err := repl.Repl()

		if err != nil {
			print(fmt.Sprintf("ERROR: %s", err))
			continue
		}

		for _, obj := range objs {
			print(fmt.Sprintf("%v", obj))
		}
	}
}

func print(msg string) {
	io.WriteString(os.Stdout, fmt.Sprintf("%s\n", msg))
}
