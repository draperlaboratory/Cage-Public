## Define the two arguments that the 'template' expects
AS_JARS := LogSecret.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

LogSecret.jar: LogSecret.class
	jar -cvfe LogSecret.jar LogSecret $^
