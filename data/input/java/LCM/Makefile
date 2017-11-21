## Define the two arguments that the 'template' expects
AS_JARS := LCM.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

LCM.jar: LCM.class StaticGCD.class
	jar -cvfe LCM.jar LCM $^
