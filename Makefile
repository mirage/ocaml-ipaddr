.PHONY: all clean

all:
	dune build

test:
	dune runtest

clean:
	dune clean
