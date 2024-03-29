MAKEFLAGS += --no-print-directory

gosch: $(shell find . -name "*.go")
	go build

.PHONY: test
test:
	go vet ./...
	go test ./...
	@ $(MAKE) integration-tests

.PHONY: lint
lint:
	golangci-lint run

.PHONY: integration-tests
integration-tests: gosch
	cd examples/the-little-schemer/ && ../../gosch run-all.scm

.PHONY: cov
cov:
	go test -coverprofile=coverage.out ./...
	go tool cover -html=coverage.out -o coverage.html

.PHONY: staticcheck
staticcheck:
	# go get honnef.co/go/tools/cmd/staticcheck
	staticcheck ./...

.PHONY: cycl
cycl:
	# go get github.com/fzipp/gocyclo/cmd/gocyclo
	gocyclo -top 10 .

.PHONY: cogn
cogn:
	# go get github.com/uudashr/gocognit/cmd/gocognit
	gocognit -top 10 .

.PHONY: benchmarks
benchmarks:
	go test -bench=. ./...

.PHONY: repl
repl: gosch
	@ ./gosch

.PHONY: fmt
fmt:
	go fmt ./...

.PHONY: lines
lines:
	@ find . -type f \( -name "*.go" -not -name "*_test.go" \) -exec cat {} \; | grep . | wc -l

.PHONY: clean
clean:
	rm -rf *.out *.html ./gosch
