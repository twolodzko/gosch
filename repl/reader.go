package repl

import (
	"bufio"
	"errors"
	"io"
)

type blockReader struct {
	*bufio.Reader
	openBlocksCount int
	isQuoted        bool
	isEscaped       bool
}

func (repl *Repl) read() (string, error) {
	var (
		err       error
		out, line string
	)

	reader := blockReader{repl.reader, 0, false, false}

	for {
		line, err = reader.ReadString('\n')

		if err != nil && err != io.EOF {
			return out, err
		}

		out += line

		if reader.shouldStop(line) || err == io.EOF {
			break
		}
	}

	switch {
	case reader.openBlocksCount > 0:
		err = errors.New("missing closing bracket")
	case reader.openBlocksCount < 0:
		err = errors.New("missing opening bracket")
	}

	return out, err
}

func (reader *blockReader) shouldStop(line string) bool {
	for _, r := range line {

		switch r {
		case '"':
			if !reader.isEscaped {
				reader.isQuoted = !reader.isQuoted
			}
		// comment - ignore rest of the line
		case ';':
			return false
		// list - wait till closing bracket
		case '(', '[':
			reader.openBlocksCount++
		case ')', ']':
			reader.openBlocksCount--

			if reader.openBlocksCount <= 0 {
				return true
			}
		}

		if r == '\\' && !reader.isEscaped {
			reader.isEscaped = true
		} else {
			reader.isEscaped = false
		}
	}

	return reader.openBlocksCount <= 0 && !reader.isQuoted
}
