## Define the two arguments that the 'template' expects
AS_JARS := MyList.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

MyList.jar: MyList.class
	jar -cvfe MyList.jar MyList $^
