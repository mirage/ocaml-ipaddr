.PHONY: all test build install reinstall uninstall

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
