.PHONY: all test build install reinstall uninstall clean

all: build test install

build:
	ocaml setup.ml -build

test:
	ocaml setup.ml -test

install:
	ocaml setup.ml -install

reinstall:
	ocaml setup.ml -reinstall

uninstall:
	ocaml setup.ml -uninstall

clean:
	ocaml setup.ml -clean
