LIBS=-libs graph,str,nums

LIBPATH=-package ocamlgraph -package unix

OPTS=${PP_OPTS} -use-ocamlfind

default: all

all: testParser

testParser: main.ml
	ocamlbuild ${OPTS} ${LIBPATH} ${LIBS} main.native

clean:
	ocamlbuild -clean
	-rm -f *\~

PHONY: clean
