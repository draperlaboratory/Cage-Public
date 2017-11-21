## Define the two arguments that the 'template' expects
AS_JARS := ExpList.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

ExpList.jar: ExpList.class
	jar -cvfe ExpList.jar ExpList $^
