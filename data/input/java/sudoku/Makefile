## Define the two arguments that the 'template' expects
AS_JARS := Sudoku.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Sudoku.jar: Board.class DepthFirst.class Move.class Location.class
	jar -cvfe Sudoku.jar DepthFirst $^
