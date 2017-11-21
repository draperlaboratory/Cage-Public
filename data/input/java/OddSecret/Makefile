## Define the two arguments that the 'template' expects
AS_JARS := OddSecret.jar OddSecret2.jar
AS_JBC = $(subst .jar,.jbc,$(AS_JARS))

## Include the template, with it's standard clean rules and such
include ../Makefile.JavaTemplate

OddSecret.jar: OddSecret.class
	jar -cvfe OddSecret.jar OddSecret $^

OddSecret2.jar: OddSecret2.class OddSecret.class
	jar -cvfe OddSecret2.jar OddSecret2 $^
