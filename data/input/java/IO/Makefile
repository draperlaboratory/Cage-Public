## Define the two arguments that the 'template' expects
AS_JARS := IO.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

IO.jar: IO.class
	jar -cvfe IO.jar IO $^
