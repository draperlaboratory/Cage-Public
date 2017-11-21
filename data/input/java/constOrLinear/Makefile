## Define the two arguments that the 'template' expects
AS_JARS := ConstOrLinear.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

ConstOrLinear.jar: ConstOrLinear.class
	jar -cvfe ConstOrLinear.jar ConstOrLinear $^
