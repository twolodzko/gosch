package main

import (
	"fmt"
	"io"
	"log"
	"os"

	"github.com/twolodzko/gosch/envir"
	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/repl"
)

const prompt string = "> "

func main() {
	if len(os.Args) == 2 {
		if os.Args[1] == "-h" || os.Args[1] == "--help" {
			printHelp()
			return
		}
		env := envir.NewEnv()
		sexprs, err := eval.LoadEval(os.Args[1], env)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
		if len(sexprs) > 0 {
			fmt.Printf("%v\n", sexprs[len(sexprs)-1])
		} else {
			fmt.Printf("%v\n", nil)
		}
		return
	}

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

func printHelp() {
	fmt.Printf("%s [script]\n", os.Args[0])
	fmt.Println()
	fmt.Println("Usage:")
	fmt.Printf("  %s             start REPL\n", os.Args[0])
	fmt.Printf("  %s script.scm  evaluate script.scm\n", os.Args[0])
	fmt.Printf("  %s -h,--help   display help\n", os.Args[0])
}
