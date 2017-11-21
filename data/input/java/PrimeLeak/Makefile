## Define the two arguments that the 'template' expects
AS_JARS := PrimeLeak.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

PrimeLeak.jar: PrimeLeak.class
	jar -cvfe PrimeLeak.jar PrimeLeak $^
