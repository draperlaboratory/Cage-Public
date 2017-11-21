all: main.pdf

%.pdf : %.tex
	pdflatex $^

.PHONY: clean dist-clean

clean :
	-rm *.aux
	-rm *.log
	-rm *\~
	-rm \#*

dist-clean : clean
	-rm *.pdf
