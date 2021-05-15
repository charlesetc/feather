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

.PHONY: test promote utop example install build
