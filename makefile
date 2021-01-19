test:
	dune runtest

promote:
	dune runtest --auto-promote

example:
	@dune exec ./example.exe

make install:
	@dune build @install
	@dune install

build:
	@dune build
