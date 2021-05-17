build:
	dune build

test:
	dune runtest

clean:
	dune clean

fmt:
	dune build @fmt --auto-promote

.PHONY: build test clean
