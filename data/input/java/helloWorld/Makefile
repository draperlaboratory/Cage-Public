## Define the two arguments that the 'template' expects
AS_JARS := HelloWorld.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

HelloWorld.jar: HelloWorld.class
	jar -cvfe HelloWorld.jar HelloWorld $^
