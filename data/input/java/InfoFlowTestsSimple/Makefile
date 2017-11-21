## Define the two arguments that the 'template' expects
AS_JARS := Test.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

Test.jar: Test.class Foo.class Faz.class
	jar -cvfe $@ Test $^
