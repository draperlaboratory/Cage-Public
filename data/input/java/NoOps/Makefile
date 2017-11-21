## Define the two arguments that the 'template' expects
AS_JARS := NoOp.jar NoOp2.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

NoOp.jar: NoOp.class
	jar -cvfe $@ NoOp $^

NoOp2.jar: NoOp2.class
	jar -cvfe $@ NoOp2 $^
