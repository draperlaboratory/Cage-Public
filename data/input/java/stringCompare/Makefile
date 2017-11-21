## Define the two arguments that the 'template' expects
AS_JARS := TestString.jar TestInt.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

TestString.jar : TestString.class
	jar -cvfe TestString.jar TestString $^

TestInt.jar : TestInt.class
	jar -cvfe TestInt.jar TestInt $^
