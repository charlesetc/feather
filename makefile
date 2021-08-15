test:
	dune runtest

promote:
	dune runtest --auto-promote

utop:
	dune utop

example:
	@dune exec ./example.exe

install:
	@dune build @install
	@dune install

build:
	@dune build

docs:
	@dune build @doc
	@rsync -a _build/default/_doc/_html/ docs/

doc: docs

.PHONY: test promote utop example install build doc docs
