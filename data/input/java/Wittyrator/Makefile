## Define the two arguments that the 'template' expects
AS_JARS := Wittyrator.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Wittyrator.jar: Wittyrator.class IntRef.class
	jar -cvfe Wittyrator.jar Wittyrator $^
