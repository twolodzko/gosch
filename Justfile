test: unit-test integration-test

unit-test:
    go vet ./...
    go test ./...

lint:
    golangci-lint run

integration-test: build
    cd examples/the-little-schemer/ && ../../gosch run-all.scm

cov:
    go test -coverprofile=coverage.out ./...
    go tool cover -html=coverage.out -o coverage.html

staticcheck:
    # go get honnef.co/go/tools/cmd/staticcheck
    staticcheck ./...

cycl:
    # go get github.com/fzipp/gocyclo/cmd/gocyclo
    gocyclo -top 10 .

cogn:
    # go get github.com/uudashr/gocognit/cmd/gocognit
    gocognit -top 10 .

benchmarks:
    go test -bench=. ./...

build:
    go build

repl: build
    @ ./gosch

fmt:
    go fmt ./...

lines:
    @ find . -type f \( -name "*.go" -not -name "*_test.go" \) -exec cat {} \; | grep . | wc -l

clean:
    rm -rf *.out *.html ./gosch
