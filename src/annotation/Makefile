
all: poly_parser_test annotation_parser_test

poly_parser_test: polyLexer.mll polyParser.mly testPolyParser.ml
	ocamlbuild testPolyParser.native

annotation_parser_test: AnnotationParser.ml polyLexer.mll polyParser.mly testAnnotationParser.ml
	ocamlbuild -pkg yojson testAnnotationParser.native

.PHONY : clean

clean :
	ocamlbuild -clean
	-rm *\~
	-rm *\#
