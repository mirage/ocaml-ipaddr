.PHONY: all

all: ipaddr.cmo test_ipaddr.ml
	ocamlfind ocamlc -o test_ipaddr -package oUnit -I ../lib \
		../lib/ipaddr.cmo -linkpkg test_ipaddr.ml
	./test_ipaddr
	@echo
	@rm ../lib/ipaddr.cmi ../lib/ipaddr.cmo

ipaddr.cmi: ../lib/ipaddr.mli
	ocamlfind ocamlc -c ../lib/ipaddr.mli

ipaddr.cmo: ipaddr.cmi ../lib/ipaddr.ml
	ocamlfind ocamlc -c -I ../lib ../lib/ipaddr.ml
