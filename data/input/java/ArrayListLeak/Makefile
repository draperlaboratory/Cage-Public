## Define the two arguments that the 'template' expects
AS_JARS := ArrayListLeak.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

ArrayListLeak.jar: ArrayListLeak.class ArrayList.class
	jar -cvfe ArrayListLeak.jar ArrayListLeak $^
