package main

import (
	"flag"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/twolodzko/gosch/eval"
	"github.com/twolodzko/gosch/repl"
	"github.com/twolodzko/gosch/scheme"
)

const prompt string = "> "

func main() {
	var showHelp bool

	flag.BoolVar(&showHelp, "help", false, "show help")
	flag.Parse()

	if showHelp {
		printHelp()
		return
	}
	// TODO: use default env
	if flag.NArg() > 0 {
		evalFiles()
	} else {
		startRepl()
	}
}

func evalFiles() {
	env := scheme.DefaultEnv()
	for _, filename := range flag.Args() {
		sexprs, err := eval.LoadEval(filename, env)
		if err != nil {
			log.Fatalf("ERROR: %v\n", err)
		}
		if len(sexprs) > 0 {
			fmt.Printf("%v\n", sexprs[len(sexprs)-1])
		} else {
			fmt.Printf("%v\n", nil)
		}
	}
}

func startRepl() {
	env := scheme.DefaultEnv()
	repl := repl.NewRepl(os.Stdin, env)

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

func printHelp() {
	fmt.Printf("%s FLAGS [script]\n", os.Args[0])
	fmt.Println()
	fmt.Println("Usage:")
	flag.PrintDefaults()
}

func print(msg string) {
	_, err := io.WriteString(os.Stdout, fmt.Sprintf("%s\n", msg))
	if err != nil {
		log.Fatal(err)
	}
}
