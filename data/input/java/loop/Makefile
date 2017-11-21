## Define the two arguments that the 'template' expects
AS_JARS := Loop.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Loop.jar: Loop.class List.class Nil.class Node.class
	jar -cvfe Loop.jar Loop $^
